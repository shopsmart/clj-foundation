(ns clj-foundation.patterns
  (:require [schema.core :as s :refer [=> =>*]]
            [potemkin :refer [def-map-type]])
  (:gen-class))


;; Schema/type utilities ------------------------------------------------------------------------------------

(defn types
  "Returns a schema that matches the listed surface types only (e.g.: primitive or collection types but
  not contents)."
  [& schemas]
  (let [type-names (map #(.getName %) schemas) ;; FIXME: This only works for schemas that are (instance? java.lang.Class)
        type-name-string (apply str (interpose ", " type-names))]
    (s/named (apply s/cond-pre schemas) type-name-string)))


(def KeywordValuePairs
  "A schema for keyword-value pairs.  Currently unsupported by Schema so defining once here so that once support lands,
  there is only one place to change to enforce it everywhere."
  [])


(s/defn get-package :- s/Str
  "Returns the package name for the specified Class"
  [clazz :- Class]
  (->> (.split (.getName clazz) "\\.")
       reverse
       rest
       reverse
       (interpose ".")
       (apply str)))


(s/defn get-class-name :- s/Str
  "Returns the unqualified class name for the specified Class"
  [clazz :- Class]
  (->> (.split (.getName clazz) "\\.")
       reverse
       first
       (apply str)))


(defn arity
 "Returns the maximum parameter count of each invoke method found by reflection
  on the input instance. The returned value can be then interpreted as the arity
  of the input function. The count does NOT detect variadic functions."
  [f]
  (let [invokes (filter #(= "invoke" (.getName %1)) (.getDeclaredMethods (class f)))]
    (apply max (map #(alength (.getParameterTypes %1)) invokes))))


;; The singleton pattern ------------------------------------------------------------------------------------

(defmacro def-singleton-fn
  "Define a function whose return value is initilized once by executing body and that returns
  that same value on every subsequent call.  A Clojure implementation of the Singleton pattern."
  [name & body]
  `(def ~name (memoize (fn [] ~@body))))


;; The Nothing object / collection utilities ----------------------------------------------------------------

(defn any?
  "Returns truthy if any element in coll satisfies predicate."
  [predicate coll]
  (not-empty (filter predicate coll)))


(definterface IWhy
  (why []))

(def-map-type Nothing [string-value reason]
  IWhy
  (get [_ k default-value]
       default-value)
  (assoc [_ k v] (assoc {} k v))
  (dissoc [_ k] {})
  (keys [_] nil)
  (meta [_] {})
  (with-meta [this mta] this)
  (toString [this] string-value)
  (why [_] reason))


(def nothing
  "Nothing is the value to use when there is nothing to pass or return.  Note that Nothing
  acts like an empty map in a collection context and like an empty string in a string context.
  This has the following implications:

  * You can use it as a result in mapcat when you want nothing appended to the output collection.
  * You can cons a value into nothing, resulting in a seq.
  * You can assoc values into a nothing, resulting in a map.
  * You can conj vector pairs into a nothing, resulting in a map.
  * You can concatinate it with other strings using the str function, adding nothing to the other strings.
  * etc..."
  (Nothing. "" {}))


(def NO-RESULT-ERROR
  "An instance of Nothing intended for use as a generic error result.  It is a separate instance
  from 'nothing' because returning 'nothing' might not be an error.  This value is useful for functions
  used within map / mapcat / filter (etc...) chains where it is useful to have an error value that
  behaves like the identity value for a collection.

  However, unlike the nothing value, in a string context, NO-RESULT-ERROR returns an error message.

  NO-RESULT-ERROR is treated as a failure by the failure? multimethod in the errors package."
  (Nothing. "No result error {}" {}))


(s/defn no-result :- Nothing
  "Create custom Nothingness with a specified (.toString n) and (.why n) value."
  [string-value :- s/Str
   reason       :- s/Any]
  (Nothing. string-value reason))


(def use-defaults
  "A synonmym for nothing for use in parameter lists when passing nothing really means for the
  function to use default values."
  nothing)


(s/defn Nothing! :- Class
  "Return the Nothing type (mainly for use in Schemas)"
  []
  Nothing)


(s/defn something? :- s/Any
  "Returns value if value is not nothing ; else returns nil."
  [value :- s/Any]
  (if (instance? Nothing value)
    nil
    value))


(s/defn nothing->identity :- s/Any
  "Takes nil or Nothing to the specified identity value for the type and computation in context,
  otherwise returns value.  An identity value can be applied to a value of the given type under the
  operation in context without affecting the result.  For example 0 is the identity value for rational
  numbers under addition.  The empty string is the identity value for strings under concatination.

  Note that Nothing is already an identity value for maps and seqs.  This function is only useful
  for types where the Nothing type is ill-behaved (e.g.: Strings, Numbers, ...) for a given operation.

  Another name for this concept is the monadic zero for the type/operation."
  [identity-value :- s/Any, value :- s/Any]

  (if (something? value)
    value
    identity-value))


;; Retain intermediate steps in a map -----------------------------------------------------------------------

(defmacro let-map
  "A version of let that returns its local variables in a map.
  If a result is computed in the body, and that result is another map,
  let-map returns the result of conj-ing the result map into the let
  expression map.  Otherwise it returns a vector containing the let
  expression  map followed by the result."
  [var-exprs & body]
  (let [vars (map (fn [[var form]] [(keyword var) var]) (partition 2 var-exprs))
        has-body (not (empty? body))]
    `(let [~@var-exprs
           result# (do ~@body)
           mapvars# (into {} [~@vars])]
       (if ~has-body
         (if (map? result#)
           (conj mapvars# result#)
           [mapvars# result#])
         mapvars#))))


(defmacro letfn-map
  "A version of letfn that returns its functions in a map.
  If a result is computed in the body, and that result is another map,
  fn-map returns the result of conj-ing the result map into the function
  map.  Otherwise it returns a vector containing the function map
  followed by the result."
  [fn-exprs & body]
  (let [fn-refs (map (fn [f] [(keyword (first f)) (first f)]) fn-exprs)
        has-body (not (empty? body))]
    `(letfn [~@fn-exprs]
       (let [result# (do ~@body)
             mapfns# (into {} [~@fn-refs])]
         (if ~has-body
           (if (map? result#)
             (conj mapfns# result#)
             [mapfns# result#])
           mapfns#)))))
