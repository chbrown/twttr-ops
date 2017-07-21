(ns twttr.ops
  "High-level Twitter API helpers"
  (:require [clojure.string :as str]
            [twttr.api :as api]))

(defn- create-status-placeholder
  "Create a status placeholder, with only :id and :id_str fields, indicating
  a status that has been deleted or perhaps never existed"
  [id-str]
  {:id (Long/parseLong id-str) :id_str id-str})

(defn- statuses-lookup-page
  "Fetch a single page of statuses, including placeholders for missing statuses.
  The ids argument should be a sequence of at most 100 numbers or strings."
  [credentials ids]
  ; statuses/lookup.json?map=true&id=8100465728375,1 returns JSON like:
  ; {"id":{"8100465728375":{"created_at":<...>},"1":null}}
  (let [params {:id (str/join "," ids) :map true}
        statuses-lookup-map (:id (api/statuses-lookup credentials :params params))]
    (for [[id status] statuses-lookup-map]
      (merge (create-status-placeholder (name id)) status))))

(defn statuses-lookup
  "Serially lookup all statuses indicated by ids (a sequence of numbers or strings),
  grouping into batches of 100, returning placeholders (with :id and :id_str keys)
  for deleted/missing statuses."
  [credentials ids]
  (mapcat #(statuses-lookup-page credentials %) (partition-all 100 ids)))

(defn concat-replied-to-statuses
  "For all of the given statuses, backtrack through each of the entire conversations
  and fetch the original tweets into a single list of statuses"
  [credentials leaf-statuses]
  (loop [statuses leaf-statuses]
    (let [ids (->> statuses (map :id) set)
          replied-to-ids (->> statuses (map :in_reply_to_status_id) (remove nil?) set)]
      (if-let [missing-ids (seq (remove ids replied-to-ids))]
        ; there are ids left to get; go get them!
        (recur (concat statuses (statuses-lookup credentials missing-ids)))
        ; else return the current statuses
        statuses))))

(defn- users-lookup-page
  "Fetch a single page of up to 100 users. Twitter will throw a 403 'Too many
  terms specified in query' if the total number user-ids together with
  screen-names is more than 100."
  [credentials {:keys [user-ids screen-names]}]
  (let [params {:user_id (str/join "," user-ids) :screen_name (str/join "," screen-names)}
        users (api/users-lookup credentials :params params)]
    ; Twitter is weird and responds HTTP 404 if no screen names can be resolved to users;
    ; but if at least one can be resolved, it returns a list of those that can, and simply
    ; omits those that can't
    (when (not= 404 (:status (meta users)))
      users)))

(defn users-lookup
  "Serially lookup all users corresponding to user-ids and screen-names,
  grouping into batches of 100, returning a sequence of found users
  (missing / deleted / suspended users are omitted from the output)"
  [credentials {:keys [user-ids screen-names]}]
  ; combine user-ids and screen-names so that they can be partitioned
  (let [user-ids-maps (map #(hash-map :user-ids [%]) user-ids)
        screen-names-maps (map #(hash-map :screen-names [%]) screen-names)]
    (->> (concat user-ids-maps screen-names-maps)
         (partition-all 100)
         ; we now have a list of lists that are at most 100 items long, where
         ; each item is a map like {:screen-names ["TwitterEng"]} or {:user-ids [145]}
         (mapcat #(users-lookup-page credentials (apply merge-with concat %))))))

(defn user-timeline
  "Get the most recent tweets that came after after-id, but before before-id,
  exclusive, from most recent to longer ago (which is the only supported ordering)"
  [credentials screen-name & {:keys [after-id   before-id]
                              :or   {after-id 1 before-id Long/MAX_VALUE}}]
  (lazy-seq
    ; max-id is inclusive, so we have to exclude the last minimum id, thus the dec
    (when-let [page (->> {:screen_name screen-name
                          :since_id after-id
                          :max_id (dec before-id)
                          :count 200
                          :include_rts 1}
                         (api/statuses-user-timeline credentials :params)
                         seq)]
      (concat page (user-timeline credentials screen-name
                                       :after-id after-id
                                       :before-id (apply min before-id (map :id page)))))))

(defn fill-user-timeline
  "Retrieve as many tweets as possible that have been published by screen-name
  more recently than the most recent status in statuses."
  [credentials screen-name statuses]
  ; going forward from what we've now got is kind of useless since
  ; statuses/user_timeline.json returns most recent tweets no matter what,
  ; as long as they come before :max_id, so we always start at the current
  ; time and then work backward in time as far as needed / as far as possible
  (let [latest-id (apply max 1 (map :id statuses))]
    (concat statuses (user-timeline credentials screen-name :after-id latest-id))))

(defn iterate-cursor
  "Recursively call (api-function credentials :params params),
  updating params each call with the next_cursor value from the response."
  [credentials api-function params]
  (lazy-seq
    (when-not (= 0 (:cursor params)) ; (= 0 x) is not the same as (zero? x)
      (let [{:keys [next_cursor] :as page} (api-function credentials :params params)
            next-params (assoc params :cursor next_cursor)]
        (cons page (iterate-cursor credentials api-function next-params))))))

(defn followers
  "Return a lazy seq of all user IDs that the user indicated by `params` is followed by.
  `params` should contain either `:screen_name` or `:user_id`."
  [credentials params]
  {:pre [(or (contains? params :user_id)
             (contains? params :screen_name))]}
  (mapcat :ids (iterate-cursor credentials api/followers-ids params)))

(defn followers-users
  "Like `followers` but returns fully-hydrated user objects via `users-lookup`"
  [credentials params]
  (users-lookup credentials {:user-ids (followers credentials params)}))

(defn friends
  "Return a lazy seq of all user IDs that the user indicated by `params` is following.
  `params` should contain either `:screen_name` or `:user_id`."
  [credentials params]
  {:pre [(or (contains? params :user_id)
             (contains? params :screen_name))]}
  (mapcat :ids (iterate-cursor credentials api/friends-ids params)))

(defn friends-users
  "Like `friends` but returns fully-hydrated user objects via `users-lookup`"
  [credentials params]
  (users-lookup credentials {:user-ids (friends credentials params)}))

(defn search
  "call like (ops/search {:q \"to:NPR\"})
  See https://dev.twitter.com/rest/public/search for the q param syntax"
  [credentials params]
  (lazy-seq
    (let [params (assoc params :include_entities 1
                               :result_type "recent"
                               :count 100)
          {:keys [statuses search_metadata]} (api/search-tweets credentials :params params)
          ; (:next_results search_metadata) provides the next fully serialized query string,
          ; but then we have to parse it to merge it back in :(
          earliest-id (apply min Long/MAX_VALUE (map :id statuses))
          ; set max_id to proceed
          next-params (assoc params :max_id (dec earliest-id))]
      (concat statuses
        (when (contains? search_metadata :next_results) ; (seq statuses)
          (search credentials next-params))))))
