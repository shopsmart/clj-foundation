(ns clj-foundation.templates
  "Implements variable substitution into strings.  Variables are specified in the form
  ${VAR}.  "
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [schema.core :as s :refer [=> =>* defschema]]
            [clj-foundation.patterns :as p :refer [types nothing]]
            [clj-foundation.errors :as err :refer [try*]])
  (:gen-class))


(defn subst-map<-
  "Convert a vector like found in a variadic parameter into a substitution map.  Supported cases:

  * The variadic parameter is empty -> return {}.
  * The variadic parameter contains a map -> return the map.
  * The variadic parameter is a sequence of keys and values -> convert to map."

  [variadic-parameter]
  (cond
    (empty? variadic-parameter) {}
    (map? (first variadic-parameter)) (first variadic-parameter)
    (odd? (count variadic-parameter)) (throw (IllegalArgumentException.
                                              "Expected an even number of arguments to make into kv pairs."))
    :else (apply assoc {} variadic-parameter)))


(defn seq->comma-string
  "Generally-useful function for turning a seq into a comma-separated string."
  [v]
  (if (sequential? v)
    (str/join ", " v)
    (throw (IllegalArgumentException. (str (type v) " isn't a seq.")))))


(s/defn resolve-var :- s/Any
  "Resolves (template) variable values using the following precedence:

  * (System/getProperty)
  * (System/getenv)
  * default-substitutions map

  Options are key/value pairs.

  If Options contains :partial-resolve true, returns err/nothing if a variable cannot
  be resolved.  Otherwise, if a variable cannot be resolved, throws ExceptionInfo."

  [default-substitutions :- {s/Any s/Any}
   var-name              :- s/Keyword
   & options             :- [s/Any]]

  (let [opts (subst-map<- options)
        lookup-string (name var-name)
        result (or
                (System/getProperty lookup-string)
                (get (System/getenv) lookup-string nil)
                (var-name default-substitutions))]
    (cond
      (and (not result) (:partial-resolve opts)) nothing
      (not result)                               (throw (ex-info (str "Not found: '"
                                                                      var-name
                                                                      "'")
                                                                 default-substitutions))
      :else                                      result)))



(defn- subst-var-match->subst-var-keyword
  "Given a substitution variable match from re-seq, return the keyward corresponding
  to the variable's name."
  [re-seq-match]
  (-> re-seq-match
      second
      keyword))


(s/defn try-resolve-var :- s/Any
  "Resolve variable using resolve-var semantics.  If :partial-resolve true is in options,
  unsuccessful variable resolutions are converted back to the variable's string representation
  and returned."
  [substitutions :- {s/Keyword s/Any}
   var           :- s/Keyword
   & options     :- [s/Any]]
  (-> (apply resolve-var substitutions var options)
      (err/value-or (fn [_] (str "${" (name var) "}")))))


(defn- re-seq-key->value
  "Returns a function closing over substitution-map for translating
  lookup keys (from the second element of re-seq tuples) to values
  using templates/resolve-var.  If resolve-var returns err/nothing,
  returns the original lookup key text.

  If :partial-resolve true is specified in options, resolves as many
  variables as possible and returns a partially-substituted string if
  any variable values cannot be resolved."
  [substitution-map & options]
  (fn [key]
    (let [lookup-keyword (subst-var-match->subst-var-keyword key)]
      (apply try-resolve-var substitution-map lookup-keyword options))))


(def ^:private var-subst-regex #"\$\{(.*?)\}")


(s/defn parameters<- :- [s/Any]
  "Parse template string and return the parameter list variables in the order in
  which they are defined in the string."
  [template :- s/Str]
  (map subst-var-match->subst-var-keyword (re-seq var-subst-regex template)))


(s/defn parameter-list<- :- [s/Any]
  "Generates a parameter list seq with variable values in the order in which variables are
  encountered in variable-source using resolve-var.

  A parameter source is either a template string or a seq of variables in the order in
  which their values must be applied to some future function.

  If :partial-resolve true is specified in options, resolves as many
  variables as possible.  Unresolved variables are returned as ${var-name}."
  [parameter-source :- (types String clojure.lang.ISeq)
   substitution-map :- {s/Any s/Any}
   & options        :- [s/Any]]
  (let [re-match->value (apply re-seq-key->value substitution-map options)]
    (cond
      (string? parameter-source)     (map re-match->value (re-seq var-subst-regex parameter-source))
      (sequential? parameter-source) (map #(apply try-resolve-var substitution-map % options) parameter-source)
      :else                          (throw (IllegalStateException. (str "Should never get here: " (type parameter-source)))))))


(defn- format-vars
  "Accept a resource string and a map containing variable names mapped
  to values.  The resource string is converted to a format string suitable
  for the 'format' function.  It then computes the actual argument
  values that should be passed as parameters to 'format' corresponding
  with each %s that was substituted.

  If :partial-resolve true is specified in options, resolves as many
  variables as possible and returns a partially-substituted string if
  any variable values cannot be resolved."
  [resource-string substitution-map & options]
  (let [fstr (str/replace resource-string #"%" "%%")
        fstr (str/replace fstr var-subst-regex "%s")
        fargs (apply parameter-list<- resource-string substitution-map options)]
    [fstr fargs]))


(s/defn sql-vars :- [s/Str []]
  "Converts resource string using substitution-map into a tuple vector [String [binding arguments]]
  where the String is the SQL with binding question marks replacing the template variables and
  the binding arguments vector containing the variable name corresponding to each ? binding."
  [resource-string]
  (let [sql (str/replace resource-string var-subst-regex "?")
        sql-arguments (parameters<- resource-string)]
    [sql sql-arguments]))



(defn subst<-
  "Given a string containing variables of the form ${variable-name}
  and a map with keywords corresponding to the variable names/values
  where the values specified in the map represent default values
  to be used if the variable's value is not discovered via another
  method.

  Values are derived (in the following order of precedence) from:

  * Java system properties
  * O/S environment variables
  * Values in substitution-map

  If a value cannot be resolved, throws ExceptionInfo."

  [variable-string & substitutions]
  (let [substitution-map (subst-map<- substitutions)
        [fstr fargs] (format-vars variable-string substitution-map)]
    (apply format fstr fargs)))


(defn partial-subst<-
  "Like subst<- but returns a partially-substituted string if values
  cannot be resolved.  Use interpolation-vars to discover what variables
  have not been substituted yet."

  [variable-string & substitutions]
  (let [substitution-map (subst-map<- substitutions)
        [fstr fargs] (format-vars variable-string substitution-map :partial-resolve true)]
    (apply format fstr fargs)))


(defn interpolation-vars
  "Returns the variable names that are defined in the specified string."
  [str]
  (sort (set
           (map #(keyword (second %))
                (re-seq var-subst-regex str)))))
