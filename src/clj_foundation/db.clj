(ns clj-foundation.db
  "Extensions to clojure.jdbc supporting straight string substitution into
  SQL query strings.  Substitutions are located by first searching the
  operating system environment, then the Java system environment,
  values specified at query execution time, and finally default values
  specified at query definition time.

  Variable values are specified by appending name/value pairs to either the
  query creation or execution functions in the following form:

  (my-query :var1 value1 :var2 \"value2\")

  Query execution functions may also specify a timeout value so that
  hung JDBC connections do not indefinitely hang the thread."
  (:require [clojure.tools.logging :as log]
            [clojure.string :as str]
            [clojure.java.jdbc :as db]
            [schema.core :as s :refer [=> =>* defschema Any Str]]
            [clj-foundation.errors :as err]
            [clj-foundation.io :as io]
            [clj-foundation.config :refer [read-config]]
            [clj-foundation.templates :as template]
            [clj-foundation.millis :as millis]
            [clj-foundation.errors :as errors]
            [clj-foundation.implicit-conversions :as c :refer [convert]]
            [clj-foundation.patterns :refer [nothing any? KeywordValuePairs]])

  (:gen-class))


;; Configuration -----------------------------------------------------------------------------

(def dbconfig-defaults
  "Default values used by the db library go here."
  {
   :fatal-exceptions ["only table or database owner can vacuum it"
                      "only table or database owner can analyze it"]
   :max-retries 5
   :jdbc-timeout-millis (millis/<-hours 2)
   :retry-pause-millis 1000
   :connection nothing
   })


(def JDBC-TIMEOUT-MILLIS
  "Parameter constant for overriding the default query timeout in a SQL job."
  ::JDBC-TIMEOUT-MILLIS)


(def MAX-RETRIES
  "Parameter constant for overriding the maximum number of retries in a SQL job."
  ::MAX-RETRIES)


(def RETRY-PAUSE-MILLIS
  "Parameter constant for overriding the number of milliseconds to pause between retries in a SQL job."
  ::RETRY-PAUSE-MILLIS)


(def FATAL-EXCEPTIONS
  "Specify the list of fatal exception message substrings."
  ::FATAL-EXCEPTIONS)


(def CONNECTION
  "A constant for specifying the database connection to use for the operation."
  ::CONNECTION)


(def ^:private db-setting-keys #{JDBC-TIMEOUT-MILLIS MAX-RETRIES RETRY-PAUSE-MILLIS FATAL-EXCEPTIONS CONNECTION})

(defn- constant->keyword [k] (keyword (.toLowerCase (name k))))


(def dbconfig-overrides
  "API for overridding values in the dbconfig-defaults map.  By default nothing is overridden.
  Clients can override values by swapping a map into this atom containing their own values,
  obtained via their preferred configuration mechanism."
  (atom nothing))


(defn- dbconfig
  "Return config values for the database library"
  [override-map & keys]
  (let [config-values (merge dbconfig-defaults @dbconfig-overrides override-map)]
    (apply read-config config-values keys)))


(defn dbconfig-override
  "Override or set dbconfig values for this session.  dbconfig keys (above) must identify the config
  constant to change."
  [& kvs]
  (swap! dbconfig-overrides #(merge % (convert c/Map kvs))))


(defn- varmaps<-
  "Partition kv pairs into :settings kvs and :vars kvs."
  [kvs]
  (errors/must-be "Expecting even number of parameters" (even? (count kvs)))
  (->> kvs
       (partition 2)
       (reduce
        (fn [result [k v]]
          (if (db-setting-keys k)
            (update-in result [:settings] #(conj % [(constant->keyword k) v]))
            (update-in result [:vars]     #(conj % [k v]))))
        {:settings {} :vars {}})))



;; SQL Logging -----------------------------------------------------------------------------


(s/defn censor-statement :- s/Str
  [statement :- s/Str]
  (let [replacements [[#"aws_access_key_id=[^;]+" "aws_access_key_id=XXX"]
                      [#"aws_secret_access_key=[^';]+" "aws_secret_access_key=XXX"]
                      [#"token=[^']+", "token=XXX"]]]

    (reduce (fn [cur-statement [match replacement]]
              (str/replace cur-statement match replacement))
            statement
            replacements)))


(s/defn log-censored-sql :- s/Any
  "Log a SQL command (in the case where it will be executed next).  Censors AWS
  credentials as used in the Redshift COPY command."
  [statement :- s/Str]

  (let [censored-statement (censor-statement statement)]
    (log/debug "Executing: <<EOF\n" censored-statement "\nEOF\n")))


;; Error handling ---------------------------------------------------------------------

(s/defn fatal? :- s/Bool
  "Returns true if this exception's message matches any of the substrings in fatal-exceptions and
  false otherwise."
  [e :- Throwable]
  (any? #(.contains (.getMessage e) %) (dbconfig :fatal-exceptions)))


(s/defn any-fatal-exceptions? :- s/Bool
  "If any exceptions in the exceptions seq are fatal exceptions, returns true, else returns false."
  [exceptions :- [Throwable]]
  (any? fatal? exceptions))


(defn resolve-sql
  "sql-or-resource can be either literal SQL or a path to a SQL resource file.  Resolves this parameter
  to a SQL template string.

  & default-substitutions are substitution keyword/value pairs that will be used

  a) as SQL or JDBC override parameters if the variables are named using the configuration constants
  defined above.

  b) as default values for variables defined inside SQL.  Variable values defined here may be overridden by
  Java system variables or operating system environment variables in that order.

  Any undefined variables will remain as literals in the SQL string.  Use templates/interpolation-vars
  to determine if any variables remain undefined."
  [sql-or-resource & default-substitutions]

  (let [source-sql               (if (.endsWith (.toUpperCase sql-or-resource) ".SQL")
                                   (io/read-template sql-or-resource)
                                   sql-or-resource)]
    (apply template/subst<- source-sql default-substitutions)))


(defn prepare-sql-job
  [sql & kvs]
  (let [{:keys [settings vars]} (varmap<- kvs)
        sql-with-defaults  (apply resolve-sql sql vars)
        bind-varible-names (template/interpolation-vars sql-with-defaults)
        bind-varibles (conj {} (juxt bind-varible-names (range (count bind-varible-names))))
        bound-sql (reduce (fn [str varname]
                            (str/replace
                             (re-pattern (str "\\$\\{" varname " \\}"))
                             )) sql-with-defaults bind-varible-names)]))


(defn execute-on-conn-fn
  "Returns a SQL executor that runs the specified SQL statement.
  Options are JDBC options to be passed to db/execute!"
  [& options]
  (fn [conn sql & default-substitutions]
    (let [resolved-sql (apply resolve-sql sql default-substitutions)
          statement (db/prepare-statement conn resolved-sql :timeout (dbconfig :timeout-seconds))]
      (log-censored-sql resolved-sql)
      (try
        (apply db/execute! conn [statement] options)
        (catch Throwable t
          (throw (ex-info (.getMessage t) {:sql sql} t)))))))



(defn query-on-conn-fn
  "Returns a SQL executor that runs the specified SQL query.
  Options are JDBC options to be passed to db/query

  * Note: The resulting function bypasses the job queue!"
  [& options]
  (fn [conn sql & default-substitutions]
    (log-censored-sql sql)
    (try
      (let [resolved-sql (apply resolve-sql sql default-substitutions)
            statement (db/prepare-statement conn resolved-sql :timeout (dbconfig :timeout-seconds))]
        (apply db/query conn [statement] options))
      (catch Throwable t
        (throw (ex-info (.getMessage t) {:sql sql} t))))))




(defmacro defquery
  "Define a function to query redshift using the SQL resulting from
  loading the specified resource file and substituting the subsequent
  key-value pairs for the variables defined in the resource file.  Note
  that two variables are predefined: :public is predefined to point to
  the public schema specified in the config and :staging is predefined
  to point to the staging schema.

  The resulting function can accept additional key-value pairs which
  may be used to either complete the query or to override the
  default values.

  The resulting function may be called multiple times, specifying
  additional or different variable substitutions each time if necessary."
  [function-name sql-or-resource & default-subs]
  `(def ~function-name (sql-fn (query-on-conn) ~sql-or-resource ~@default-subs)))


(defmacro defstatement
  "Define a function to execute on redshift the SQL resulting from
  loading the specified resource file and substituting the subsequent
  key-value pairs for the variables defined in the resource file.  Note
  that two variables are predefined: :public is predefined to point to
  the public schema specified in the config and :staging is predefined
  to point to the staging schema.

  The resulting function can accept additional key-value pairs which
  may be used to either complete the query or to override the
  default values.

  The resulting function may be called multiple times, specifying
  additional or different variable substitutions each time if necessary."
  [function-name sql-or-resource & default-subs]
  `(def ~function-name (sql-fn (execute-on-conn) ~sql-or-resource ~@default-subs)))


;; (defn query-fn
;;   "Create a function to query redshift using the SQL resulting from
;;   loading the specified resource file and substituting the subsequent
;;   key-value pairs for the variables defined in the resource file.  Note
;;   that two variables are predefined: :public is predefined to point to
;;   the public schema specified in the config and :staging is predefined
;;   to point to the staging schema.

;;   The resulting function can accept additional key-value pairs which
;;   may be used to either complete the query or to override the
;;   default values.

;;   The resulting function may be called multiple times, specifying
;;   additional or different variable substitutions each time if necessary."
;;   (apply sql-fn (query-on-conn)
;;          sql-or-resource
;;          default-substitutions)
;;   [sql-or-resource & default-substitutions])


;; (defn execute!-fn
;;   "Create a function to execute on redshift the SQL resulting from
;;   loading the specified resource file and substituting the subsequent
;;   key-value pairs for the variables defined in the resource file.  Note
;;   that two variables are predefined: :public is predefined to point to
;;   the public schema specified in the config and :staging is predefined
;;   to point to the staging schema.

;;   The resulting function can accept additional key-value pairs which
;;   may be used to either complete the query or to override the
;;   default values.

;;   The resulting function may be called multiple times, specifying
;;   additional or different variable substitutions each time if necessary."
;;   [sql-or-resource & default-substitutions]
;;   (apply sql-fn (execute-on-conn)
;;          sql-or-resource
;;          :public *public-schema*
;;          :staging *staging-schema*
;;          default-substitutions))



;; (defn execute!
;;   "Execute on redshift the specified SQL or the SQL in the specified
;;   resource file, substituting the subsequent key-value pairs for the
;;   variables defined in the resource file.  Note that two variables are
;;   predefined: :public is predefined to point to the public schema
;;   specified in the config and :staging is predefined to point to the
;;   staging schema."
;;   [sql-or-resource & variable-key-vals]
;;   (let [sql-statements (apply execute!-fn sql-or-resource variable-key-vals)]
;;     (sql-statements)))


;; (defn query
;;   "Query redshift using the specified SQL or the SQL in the specified
;;   resource file, substituting the subsequent key-value pairs for the
;;   variables defined in the resource file.  Note that two variables are
;;   predefined: :public is predefined to point to the public schema
;;   specified in the config and :staging is predefined to point to the
;;   staging schema."
;;   [sql-or-resource & variable-key-vals]
;;   (let [sql-query (apply query-fn sql-or-resource variable-key-vals)]
;;     (sql-query)))


(defschema QuerySubstitutions [])       ;; Really [(* Keyword Any)], but Schema doesn't know how to say that


(s/defn forall-substitutions :- {}      ; Return any map
  "query or execute! sql over a list of substitutions.

  sql-or-fn is:
    * A String pointing to a resource file with the SQL
    * A SQL string itself
    * A function (=> s/Str [{}])

  Takes an initial-result-map that will accumulate the results of executing
  SQL over all of the substitutions.

  When calling query or execute!, if the operation succeeds, call :on-success with
  the current result map, the query results, and the substitution list used as
  parameters to the sql operation.

  If the sql operation fails, call :on-failure with the current result map, the
  exception object, and the substitution list used as parameters to the sql
  operation.

  The success-function or error-function is expected to return a new map containing
  the new result of running the sequence of queries.

  If the error-function needs to abort, it can re-throw the exception.

  ;; Call like this:
  (apply for-all-substitutions queryfn result-map
    (letfn-map [(on-success [r q p] ...) (on-failure [r q p] ...)]) substitutions)

  ;; Or like this:
  (for-all-substitutions
    queryfn
    result-map
    (letfn-map
      [(on-success [r q p] ...)
      (on-failure [r q p] ...)])
    [:tab-name \"foo\"] [:tab-name \"bar\"] [:tab-name \"baz\"])"

  [execute-or-query-fn :-  (=> Any QuerySubstitutions)
   initial-result-map :-   {}
   callback-map :-         {:on-success (=> {} [(s/one {}                 "Current result")
                                                (s/one Any                "Query result")
                                                (s/one QuerySubstitutions "Query parameters")])
                            :on-failure (=> {} [(s/one {}                 "Current result")
                                                (s/one Throwable          "The exception")
                                                (s/one QuerySubstitutions "Query parameters")])}
   & substitution-lists :- [(s/optional QuerySubstitutions   "Query parameters")]]

  (reduce

   (fn [result-map substitution-list]
     (let [substitution-map (apply assoc {} substitution-list)
           success-fn (:on-success callback-map)
           error-fn (:on-failure callback-map)]
       (try
         (let [sql-result (apply execute-or-query-fn substitution-list)]
           (success-fn result-map sql-result substitution-map))
         (catch Exception e
           (error-fn result-map e substitution-map)))))

   initial-result-map
   substitution-lists))


;;-----------------------------------------------------------------------


(defn- row->kv-pair [result-row key-columns]
  [(str "'" (str/join "' && '" (mapcat (fn [col] [(str (name col) "'->'" (col result-row))]) key-columns)) "'") result-row])


(defn- add-row-to-result-map [key-columns result row]
  (let [[k v] (row->kv-pair row key-columns)
        prior-row (result k)]
    (if (contains? result k)
      (if (sequential? prior-row)
        (assoc result k (conj prior-row v))
        (assoc result k [prior-row v]))
      (assoc result k v))))

(defn- query-and-extract-key
  [query key-columns]
  (reduce (partial add-row-to-result-map key-columns) {} (query)))

(defn- extract-keys [arg-list default-keys]
  (let [possible-keys (first arg-list)
        have-keys (vector? possible-keys)
        keys (if have-keys possible-keys default-keys)
        variable-key-vals (if have-keys (rest arg-list) arg-list)]
    [keys variable-key-vals]))


;; (defn keyed-query-fn
;;   "Create a function to run the specified query or sql resource file,
;;   extract the (concatinated) key values from each result row, and transform
;;   the result into a map keyed by each row's (concatinated) key, with the
;;   row itself being the value.

;;   The initial element of the variadic parameter may optionally be a
;;   vector of keywords specifying the (concatinated) key of the table.
;;   The rest must be key-value pairs specifying default substitution
;;   variable values.

;;   If the key vector is not specified when creating the query function,
;;   it must be specified when calling the query function.

;;   If the key vector does not uniquely identify a row in the result set,
;;   keys identifying multiple rows will be associated with a vector
;;   containing the rows referenced by the key."
;;   [sql-or-resource & keys-or-variable-key-vals]
;;   (let [[key-columns variable-key-vals] (extract-keys keys-or-variable-key-vals nil)]
;;     (fn [& keys-or-extra-key-vals]
;;       (let [[key-columns extra-key-vals] (extract-keys keys-or-extra-key-vals key-columns)
;;             query (apply sql-fn (query-on-conn) sql-or-resource variable-key-vals)]
;;         (if (empty? key-columns)
;;           (throw (IllegalArgumentException. "A keyed query must specify the column(s) that make up the key.")))
;;         (query-and-extract-key #(apply query extra-key-vals) key-columns)))))


;; (defn keyed-query
;;   "Run a query returnining keyed results.

;;   The initial element of the variadic parameter must be a
;;   vector of keywords specifying the (concatinated) key of the table.

;;   The remaining parameters must be key-value pairs specifying substitution
;;   variable values for any variables defined in the SQL code or the SQL
;;   resource file."
;;   [sql-or-resource & keys-or-variable-key-vals]
;;   (let [keyed-query (apply keyed-query-fn sql-or-resource keys-or-variable-key-vals)]
;;     (keyed-query)))


;; (defmacro defkeyedquery
;;   "Define the specified query function, extract the (concatinated) key values from
;;   each result row, and transform the result into a map keyed by each row's
;;   (concatinated) key, with the row itself being the value.

;;   The key is specified as a vector of keywords describing the (concatinated) key
;;   of the query results.  This key must be the initial argument after the query itself.

;;   If the key vector does not uniquely identify a row in the result set,
;;   keys identifying multiple rows will be associated with a vector
;;   containing the rows referenced by the key."
;;   [function-name sql-or-resource & default-subs]
;;   `(def ~function-name (keyed-query-fn ~sql-or-resource ~@default-subs)))


;; (defn keystring->where-conditions
;;   "Transform a keyed query result's key string into a string containing where clause
;;   conditions usable in a sql query to retrieve any row again that utilizes the same
;;   key columns/values."
;;   [key-string]
;;   (let [key-string-parts (str/split key-string #"^'|' && '|'->'|'$")
;;         col-value-pairs (partition 2 (rest key-string-parts))
;;         where-conditions (map (fn [[col value]] (str col "='" value "'")) col-value-pairs)]
;;     (str/join " and " where-conditions)))
