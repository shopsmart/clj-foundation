(ns clj-foundation.templates
  "Implements variable substitution into strings.  Variables are specified in the form
  ${VAR}.  "
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [schema.core :as s :refer [=> =>* defschema]]
            [clj-foundation.patterns :refer [types]]
            [clj-foundation.errors :as err])
  (:gen-class))



(defn- key-to-value
  "Private implementation detail for icarrp.config.  Returns a
  function closing over substitution-map.  This function processes
  a tuple element from the re-seq function, attempting to use the
  second element in the tuple as a key to look up the corresponding
  value.  Sources considered (in order) are Java system properties,
  environment variables, and finally, values in the substitution-map."
  [substitution-map]
  (fn [key]
    (let [lookup-key (second key)
          lookup-keyword (keyword lookup-key)
          result (or
                  (System/getProperty lookup-key)
                  (get (System/getenv) lookup-key nil)
                  (lookup-keyword substitution-map))]
      (when-not result
        (throw (ex-info (str "Not found: '" (first key) "'") substitution-map)))
      result)))



(def ^:private var-subst-regex #"\$\{(.*?)\}")


(defn- format-vars
  "Private implementation detail for icarrp.config.  This function
  accepts a resource string and a map containing variable names mapped
  to values.  The resource string is converted to a format string suitable
  for the 'format' function.  It then computes the actual argument
  values that should be passed as parameters to 'format' corresponding
  with each %s that was substituted."
  [resource-string substitution-map]
  (let [fstr (str/replace resource-string #"%" "%%")
        fstr (str/replace fstr var-subst-regex "%s")
        fargs (map (key-to-value substitution-map) (re-seq var-subst-regex resource-string))]
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
