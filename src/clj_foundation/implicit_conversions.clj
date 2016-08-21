(ns clj-foundation.implicit-conversions
  (:require [clj-foundation.errors :refer [failure? must-be]]
            [clj-foundation.patterns :as patterns]))


(def Map
  "Alias for clojure.lang.IPersistentMap for use in type conversions"
  clojure.lang.IPersistentMap)

(def Vector
  "Alias for clojure.lang.PersistentVector"
  clojure.lang.PersistentVector)


(defmulti convert
  "Convert src-instance to dest-class if possible.  Returns patterns/NO-RESULT-ERROR
  on failure."
  (fn [dest-class src-instance] [dest-class (class src-instance)]))


(defmethod convert [java.io.File String] [_ str]
   (java.io.File. str))


(defmethod convert [Boolean/TYPE String] [_ str]
  (contains? #{"on" "yes" "true"} (.toLowerCase str)))


(defmethod convert [Map Vector] [_ v]
  (must-be "Vector must contain key-value pairs" (even? (count v)))
  (apply assoc {} v))


;; Synonyms...
(defmethod convert [clojure.lang.PersistentArrayMap Vector] [_ v]
  (convert Map v))

(defmethod convert [clojure.lang.PersistentHashMap Vector] [_ v]
  (convert Map v))

(defmethod convert [clojure.lang.PersistentTreeMap Vector] [_ v]
  (convert Map v))


(defmethod convert :default [_ _] patterns/NO-RESULT-ERROR)
