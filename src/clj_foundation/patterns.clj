(ns clj-foundation.patterns
  (:require [schema.core :as s :refer [=> =>*]])
  (:gen-class))


;; Schema/type utilities --------------------------------------------------------------------------------------------

(defn types
  "Returns a schema that matches the listed surface types only (e.g.: primitive or collection types but not contents)."
  [& schemas]
  (let [type-names (map #(.getName %) schemas)
        type-name-string (apply str (interpose ", " type-names))]
    (s/named (apply s/cond-pre schemas) type-name-string)))


(s/defn get-package
  "Returns the package name for the specified Class"
  [clazz :- Class]
  (->> (.split (.getName clazz) "\\.")
       reverse
       rest
       reverse
       (interpose ".")
       (apply str)))


(s/defn get-class-name
  "Returns the unqualified class name for the specified Class"
  [clazz :- Class]
  (->> (.split (.getName clazz) "\\.")
       reverse
       first
       (apply str)))


;; The singleton pattern --------------------------------------------------------------------------------------------

(defmacro def-singleton-fn
  "Define a function whose return value is initilized once by executing body and that returns
  that same value on every subsequent call.  A Clojure implementation of the Singleton pattern."
  [name & body]
  `(def ~name (memoize (fn [] ~@body))))


;; The Nothing object -----------------------------------------------------------------------------------------------

(definterface INothing)
(deftype Nothing [] INothing)

(def nothing
  "The value to use when there is no result."
  (Nothing.))


;; Retain intermediate steps in a map -------------------------------------------------------------------------------

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
  "A version of let-fn that returns its functions in a map.
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
