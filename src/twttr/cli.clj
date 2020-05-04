(ns twttr.cli
  "Command line interface to twttr-ops features"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [byte-streams :as bs]
            [byte-transforms :as bt]
            [twttr.auth :as auth]
            [twttr.api :as api]
            [twttr.ops :as ops]
            [twttr.io]
            [twttr.pprint :as pprint]
            [fancy.string :refer [pad-end]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

;; helpers

(defn- fill-user-timeline-from-file
  "Read stored statuses from `file` as JSON lines,
  infer the screen-name from the `file`'s name,
  fetch new statuses for the timeline of that user,
  fetch the replied-to statuses if `with-replied-to` is true,
  then write JSON lines back to `file`"
  [credentials ^java.io.File file with-replied-to]
  (let [screen-name (-> (.getName file) (str/split #"\.") first)
        stored-statuses (twttr.io/read-json-lines file)]
    (log/info "Read" (count stored-statuses) "stored statuses for" screen-name)
    (let [all-statuses (cond->> (ops/fill-user-timeline credentials screen-name stored-statuses)
                         with-replied-to (ops/concat-replied-to-statuses credentials))]
      (log/info "Found" (- (count all-statuses) (count stored-statuses)) "new statuses for" screen-name)
      (twttr.io/write-json-lines file all-statuses))))

(defn- reduce-user-operations-log
  "Compute the current set of users with an active link from an ordered list of
  users and delete operations. Returns a (sorted) set."
  [user-operations]
  (reduce (fn [ids {:keys [id deleted] :as user}]
            ; there are two kinds of user-operation: additions and deletions.
            (if deleted
              (disj ids id)
              (conj ids id))) (sorted-set) user-operations))

(defn- concat-new-user-operations
  "When a link is broken, a placeholder is appended, e.g., {:id 14380188, :deleted 1499462094}"
  [credentials stored-user-operations current-user-ids]
  (let [now (quot (System/currentTimeMillis) 1000)
        ; read the last-known / stored followers (this is a set)
        stored-user-ids (reduce-user-operations-log stored-user-operations)]
    (concat stored-user-operations
            ; compute additions (new followers / friends)
            (->> (ops/users-lookup credentials {:user-ids (remove stored-user-ids current-user-ids)})
                 (map #(assoc % :added now)))
            ; compute deletions (unfollowed / unfriended users)
            (->> (apply disj stored-user-ids current-user-ids)
                 (map #(hash-map :id % :deleted now))))))

(defn- update-user-operations-from-file
  "The file format goes from older to newer; state accumulates as lines are read from top to bottom."
  [credentials ^java.io.File file]
  (let [[screen-name op-name] (str/split (.getName file) #"\.")
        ops-users-fn (case op-name
                       "followers" ops/followers
                       "friends" ops/friends)
        ; ops-users-fn is either ops/followers or ops/friends;
        ; i.e., an op function taking a single :screen_name or :user_id param
        ; and returning a (lazy) seq of User IDs connected to the account specified by that param,
        ; from newest- to oldest-made connections
        stored-user-operations (twttr.io/read-json-lines file)
        log-prefix (str "@" screen-name ":")]
    (log/info log-prefix "Read" (count stored-user-operations) "user operations from file")
    (let [; fetch all live friends/followers, from oldest to newest (this is a seq)
          current-user-ids (reverse (ops-users-fn credentials {:screen_name screen-name}))]
      (log/info log-prefix "Found" (count current-user-ids) "current" op-name)
      (let [all-user-operations (concat-new-user-operations credentials stored-user-operations current-user-ids)]
        (log/info log-prefix "Appending" (- (count all-user-operations)
                                            (count stored-user-operations)) "new user operations")
        (twttr.io/write-json-lines file all-user-operations)))))

;; commands

(def ^:dynamic *credentials*)

(defn stream-command
  "Call one of the 'stream.twitter.com' API endpoints,
  remove empty lines, and write to *out*."
  [_ params]
  (let [api-function (if (some params #{:follow :track :locations})
                       api/statuses-filter
                       api/statuses-sample)]
    (log/info "Streaming with params:" params)
    (->> (api-function *credentials* :params params) ; :headers {:User-Agent "twttr"}
         (map twttr.io/write-json-str)
         (interpose "\n")
         (run! #(.write *out* ^String %)))))

(defn fill-users-timelines-command
  "Read any existing statuses from *in*, then retrieve as many as possible since then,
  get any missing tweets that were replied to, and write to *out*."
  [paths {:keys [with-replied-to]}]
  (doseq [path paths]
    (try
      (fill-user-timeline-from-file *credentials* (io/file path) with-replied-to)
      (catch clojure.lang.ExceptionInfo e
        (log/warn "Failed to fill user timeline from path:" path (str e)))
      (catch Exception e
        (log/error "Unexpected exception filling user timeline from path:" path)
        (throw e)))))

(defn update-user-operations-command
  "Infer screen_name and friends vs. followers from the filename for each path in paths,
  and update the user-operation history correspondingly,
  based on the current friends/followers of the user."
  [paths _]
  (doseq [path paths]
    (try
      (update-user-operations-from-file *credentials* (io/file path))
      (catch clojure.lang.ExceptionInfo e
        (log/warn "Failed to update user operations from path:" path (str e)))
      (catch Exception e
        (log/error "Unexpected exception updating user operations from path:" path)
        (throw e)))))

(defn fill-statuses-command
  "Read status IDs from *in*, fetch the fully-hydrated statuses, and write to *out*."
  [_ _]
  (->> (io/reader *in*)
       (line-seq)
       (ops/statuses-lookup *credentials*)
       (map #(json/write-str % :escape-unicode false))
       (run! println)))

(defn fill-users-command
  "Read user IDs from *in*, fetch the fully-hydrated users, and write to *out*."
  [_ _]
  (let [digit-characters #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
        word-characters #{\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
                          \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
                          \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \_}
        ; Infer the type of a plain string identifying a user
        ; Twitter usernames cannot be longer than 15 characters,
        ; and can only contain alphanumeric characters (letters A-Z, numbers 0-9) and underscores
        ident-key (fn [s]
                    (cond
                      ; If an identifier is all digits, assume it's a user id.
                      ; technically these could be screen names, but that's less likely.
                      (every? digit-characters s) :user-ids
                      ; Otherwise, if it meets the criteria for valid usernames, it's a screen name.
                      (and (<= (count s) 15) (every? word-characters s)) :screen-names
                      :else (throw (ex-info "Cannot infer type of user identifier" {:identifier s}))))]
    (->> (io/reader *in*)
         (line-seq)
         (group-by ident-key)
         (ops/users-lookup *credentials*)
         (map #(json/write-str % :escape-unicode false))
         (run! println))))

(defn append-replied-to-command
  "Read statuses from each path, fetch the replied_to statuses (recursively) and write back to the same file."
  [paths _]
  (doseq [path paths
          :let [file (io/file path)]]
    (let [stored-statuses (twttr.io/read-json-lines file)]
      (log/info "Read" (count stored-statuses) "stored statuses from" path)
      (let [all-statuses (ops/concat-replied-to-statuses *credentials* stored-statuses)]
        (log/info "Found" (- (count all-statuses) (count stored-statuses)) "new statuses for" path)
        (twttr.io/write-json-lines file all-statuses)))))

(defn rate-limit-status-command
  "Run application/rate_limit_status on the user credentials specified by the environment variables"
  [_ {:keys [resources]}]
  (let [params (when resources {:resources resources})
        result (api/application-rate-limit-status *credentials* :params params)]
    (println (json/write-str result :escape-unicode false))))

(defn reply-tree-command
  "Read statuses from *in* and print out in a tree format"
  [_ {:keys [screen-name]}]
  (let [statuses (twttr.io/read-json-lines *in*)]
    ; (log/info "read" (count statuses) "statuses with" (count (get replies nil)) "root statuses")
    (run! println (pprint/format-reply-tree statuses))))

(defn verify-command
  "Run account/verify_credentials on the user credentials specified by the environment variables"
  [_ _]
  (let [result (api/account-verify-credentials *credentials*)]
    (println (json/write-str result :escape-unicode false))))

;; CLI integration

(def cli-commands
  {"append-replied-to"      #'append-replied-to-command
   "fill-statuses"          #'fill-statuses-command
   "fill-users"             #'fill-users-command
   "fill-users-timelines"   #'fill-users-timelines-command
   "rate-limit-status"      #'rate-limit-status-command
   "reply-tree"             #'reply-tree-command
   "stream"                 #'stream-command
   "update-user-operations" #'update-user-operations-command
   "verify"                 #'verify-command})

(def cli-option-specs
  [["-c" "--credentials-csv CSV_FILEPATH"  "Read credentials from CSV file"]
   [nil  "--track TRACK"                   "Tracking search term"]
   [nil  "--follow USER_IDS"               "Comma-separated user IDs to stream updates for"]
   [nil  "--resources RESOURCE1,RESOURCE2" "Resource families to check rate-limit status for"]
   [nil  "--screen-name SCREEN_NAME"       "Screen name of user timeline provided on *in*"]
   [nil  "--with-replied-to"]
   ["-h" "--help"]
   ["-v" "--version"]])

(defn- exit! [code] (System/exit code))

(defn- cli-error!
  [summary & messages]
  (println "twttr/ops: cli (command line interface)")
  (println "\nCommands:")
  (let [command-width (apply max (map count (keys cli-commands)))]
    (doseq [[command command-fn] cli-commands]
      (println " " (pad-end command command-width)
               " " (-> command-fn meta :doc (str/replace #"\s+" " ")))))
  (println "\nOptions:")
  (println summary)
  (println)
  (doseq [message messages]
    (println message))
  (exit! (if messages 1 0)))

(defn -main
  [& argv]
  (let [{:keys [options arguments summary errors] :as opts} (parse-opts argv cli-option-specs)
        [command & args] arguments
        command-fn (get cli-commands command)]
    ; dispatch fatal errors
    (cond
      (:help options)    (cli-error! summary)
      (:version options) (cli-error! summary)
      (nil? command-fn)  (cli-error! summary "ArgumentError: you must supply a valid command")
      errors             (cli-error! summary (str "Argument Error: " (str/join \newline errors))))
    ; if we haven't already exited, run command
    (log/info "Calling command" (:name (meta command-fn)) "with args" args "and options" options)
    (binding [*credentials* (if-let [credentials-csv (:credentials-csv options)]
                              (-> credentials-csv auth/file->Credentials-coll auth/coll->MemoryCredentials)
                              (auth/env->UserCredentials))]
      (command-fn args options))
    (flush) ; same as (.flush *out*)
    (shutdown-agents)))
