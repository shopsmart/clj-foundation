(ns clj-foundation.patterns
  "Patterns DRY up code and provide a vocabulary for conversation.  This package helps Clojure deal with
  types from Java, makes it easier to build maps that represent abstract data types, provides functional
  programming utilities, and adds a Nothing type that behaves as an identity value for maps, sequences,
  and strings under their various concatination operations.  (For Category theorists, it's a monadic zero
  for these types under concatination and mapcat.)  It also provides Nothing constants for cases where
  Nothing means 'error', 'no result', or simply 'use default settings'."
  (:require [schema.core :as s :refer [=> =>*]]
            [clojure.string :as str]
            [potemkin :refer [def-map-type]])
  (:gen-class))


;; Schema/type utilities ------------------------------------------------------------------------------------

(defn types
  "Returns a schema that matches the listed surface types only (e.g.: primitive or collection types but
  not contents).  Only designed for schemas that are (instance? java.lang.Class)."
  [& schemas]
  (let [type-names (map #(.getName %) schemas)
        type-name-string (str/join ", " type-names)]
    (s/named (apply s/cond-pre schemas) type-name-string)))


(def KeywordValuePairs
  "A schema for keyword-value pairs.  Currently unsupported by Schema so defining once here so that once support lands,
  there is only one place to change to enforce it everywhere."
  [s/Keyword s/Any])


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
  "Nothing is the value to use when there is nothing to pass or return.  Nothing acts like an
  empty map in a collection context and like an empty string in a string context.

  This has the following implications:

  * You can use it as a result in mapcat when you want nothing appended to the output collection.
  * You can cons a value into nothing, resulting in a seq.
  * You can assoc values into a nothing, resulting in a map.
  * You can conj vector pairs into a nothing, resulting in a map.
  * You can concatinate it with other strings using the str function, adding nothing to the other strings."
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
  (when-not (instance? Nothing value)
    value))



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


;; FP Utilities --------------------------------------------------------------------------------------------


(defmacro f
  "A \"where\" form for anonymous functions.  e.g.:

  * If the body of the function is a single function call, the parens may be omitted.
    (f x y => + x y)
    (f x y & more => apply + x y more)

  * If more statements are needed, they are wrapped in an implicit (do ...)
    (f => (log/warning \"She's about to blow!\")
          (self-destruct))"
  [& all]
  (let [args# (vec (take-while #(or (vector? %1)
                                    (map? %1)
                                    (not= (name %1) "=>"))
                               all))
        argCount (count args#)
        expr (last (split-at (inc argCount) all))]
    (cond
      (seq? (first expr))    `(fn ~args# (do ~@expr))
      (vector? (first expr)) `(fn ~args# ~@expr)
      (set? (first expr))    `(fn ~args# ~@expr)
      (map? (first expr))    `(fn ~args# ~@expr)
      :else                  `(fn ~args# (~@expr)))))
