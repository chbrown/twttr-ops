(ns twttr.pprint
  (:require [clojure.string :as str]
            [twttr.io]))

(defn normalize-whitespace
  "Replace each whitespace token with a normal space"
  [string]
  (some-> string (str/replace #"\s" " ")))

(def ^:private twitter-datetime-formatter
  "Twitter uses an unusual DateTime format; matched by this custom formatter"
  (java.time.format.DateTimeFormatter/ofPattern "EEE MMM dd HH:mm:ss Z yyyy"))

(defn parse-datetime
  "Parses Twitter's unusual datetime format (e.g., the value of the `created_at` field)
  as a standard Java8 OffsetDateTime instance."
  {:test (fn []
           (assert (= "2010-04-01T01:23:45Z" (str (parse-datetime "Mon Apr 01 01:23:45 +0000 2010"))))
           ; #inst "2008-04-14T02:13:55Z"
           (assert (= "2008-04-14T02:13:55Z" (str (parse-datetime "Mon Apr 14 02:13:55 +0000 2008"))))
           (assert (= "2008-08-27T13:08:45Z" (str (parse-datetime "Wed Aug 27 13:08:45 +0000 2008")))))}
  [text]
  (java.time.OffsetDateTime/parse text twitter-datetime-formatter))

(defn normalize-status
  [status]
  (-> status
      (update :created_at parse-datetime)
      (update :text normalize-whitespace)
      (update-in [:user :name] normalize-whitespace)
      (update-in [:user :screen_name] normalize-whitespace)
      (update-in [:extended_tweet :full_text] normalize-whitespace)
      (dissoc :contributors :geo) ; these are deprecated
      ; Clojure data.json has Longs covered:
      (dissoc :id_str :in_reply_to_status_id_str :in_reply_to_user_id_str :quoted_status_id_str)
      (update :user dissoc :id_str)))

(defn- space [n] (str/join (repeat n " ")))

(defn format-status
  ([status]
   (format-status status :medium))
  ([status style]
   (cond
     ; might as well handle other stream items here
     (contains? status :delete) (str "❌ " status)
     (contains? status :limit) (str "🍰 " status) ; shortcake! why not?
     :else (try
             (let [status (normalize-status status)
                   ; see https://developer.twitter.com/en/docs/tweets/tweet-updates
                   {:keys [id created_at text favorite_count retweet_count user extended_tweet]} status
                   text (or (:full_text extended_tweet) text)
                   {:keys [screen_name]} user]
               (case style
                 :long   (twttr.io/write-json-str status)
                 :medium (format "%d %20s ⟳ %4d ♡ %4d @%s: %s"
                                 id created_at retweet_count favorite_count screen_name text)
                 :short  (str id " " "@" screen_name ": " text)))
             (catch Exception e
               (println "🎇 " e (str status))
               (throw e))))))

;; reply-to tree formatting

(defn- format-reply-branch
  "Iterate over statuses, returning lines in a tree format.
  replies is a mapping from original status ID to a list of replies to that status ID"
  ([status replies]
   (format-reply-branch status replies 0))
  ([status replies level]
   (cons (str (space (* level 2)) (format-status status :short))
         (mapcat #(format-reply-branch % replies (inc level)) (get replies (:id status))))))

(defn format-reply-tree
  "Returns list of lines"
  [initial-statuses]
  (let [statuses (dedupe (sort-by (comp - :id)) initial-statuses)
        ; Create a mapping from status IDs to replies (to that status)
        replies (reduce (fn [m status]
                          ; add the status to the list at m[in_reply_to_status_id]
                          (update m (:in_reply_to_status_id status) conj status)) {} statuses)
        ; Every status has at most one :in_reply_to_status_id value;
        ; if it has one, that is its parent; if it doesn't, it is a root node.
        ; Start with the root nodes, which are those that are in reply to nothing.
        roots (get replies nil)]
    (mapcat #(format-reply-branch % replies) roots)))
