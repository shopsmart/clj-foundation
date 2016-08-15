(ns clj-foundation.templates
  "Implements variable substitution into strings.  Variables are specified in the form
  ${VAR}.  "
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [schema.core :as s :refer [=> =>* defschema]]
            [clj-foundation.patterns :refer [types]]
            [clj-foundation.errors :as err])
  (:gen-class))


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
  * default-substitutions map"

  [default-substitutions :- {}
   var-name              :- s/Keyword]

  (let [lookup-string (name var-name)
        result (or
                (System/getProperty lookup-string)
                (get (System/getenv) lookup-string nil)
                (var-name default-substitutions))]
    (when-not result
      (throw (ex-info (str "Not found: '" (first key) "'") default-substitutions)))
    result))


(defn- re-seq-key-to-value
  "Returns a function closing over substitution-map for translating
  lookup keys (from the second element of re-seq tuples) to values
  using templates/resolve-var.

  Sources considered (in order) are Java system properties,
  environment variables, and finally, values in the substitution-map."
  [substitution-map]
  (fn [key]
    (let [lookup-key (second key)
          lookup-keyword (keyword lookup-key)]
      (resolve-var substitution-map lookup-keyword))))


(def ^:private var-subst-regex #"\$\{(.*?)\}")


(defn- format-vars
  "Accept a resource string and a map containing variable names mapped
  to values.  The resource string is converted to a format string suitable
  for the 'format' function.  It then computes the actual argument
  values that should be passed as parameters to 'format' corresponding
  with each %s that was substituted."
  [resource-string substitution-map]
  (let [fstr (str/replace resource-string #"%" "%%")
        fstr (str/replace fstr var-subst-regex "%s")
        fargs (map (re-seq-key-to-value substitution-map) (re-seq var-subst-regex resource-string))]
    [fstr fargs]))


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


(defn subst<-
  "Given a string containing variables of the form ${variable-name}
  and a map with keywords corresponding to the variable names/values
  where the values specified in the map represent default values
  to be used if the variable's value is not discovered via another
  method.

  Values are derived (in the following order of precedence) from:

  * Java system properties
  * O/S environment variables
  * Values in substitution-map"

  [variable-string & substitutions]
  (let [substitution-map (subst-map<- substitutions)
        [fstr fargs] (format-vars variable-string substitution-map)]
    (apply format fstr fargs)))



(defn interpolation-vars
  "Returns the variable names that are defined in the specified string."
  [str]
  (sort (set
           (map #(keyword (second %))
                (re-seq var-subst-regex str)))))
