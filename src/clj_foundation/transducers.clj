(ns clj-foundation.transducers
  "Utilities helping to make transducer functions out of regular functions.
  Transducers can be pipelined like thread-first or thread-list, but using regular
  function composition.

  Unlike thread-first or thread-list, a pipeline of transducers processes a single
  elemnt through all functions at a time, eliminating the need to create multiple
  intermediate collections.

  Also includes xreduce, a version of reduce that returns a transducer function
  (this function is seemingly missing from the standard library)."
  (:require [clj-foundation.patterns :refer [arity nothing]]
            [clj-foundation.errors :refer [must-be]])
  (:gen-class))


(defn xmapcat
  "Return a transducer calling (mapcat f) for an arbitrary f.  Results are interpreted
  as follows:

  * If (f x) is a map, seq, or set, it is returned as-is.
  * If (f x) is nil, patterns/nothing is returned.
  * Otherwise, the result is wrapped in a vector and returned."
  [f]
  ;; Note that calling mapcat without an input collection returns a trunsducer
  (mapcat (fn [x]
            (let [r (f x)]
              (cond
                (or (map? r)
                    (sequential? r)
                    (set? r))        r
                (nil? r)             nothing
                :else                [r])))))


(defn xreduce
  "A version of reduce that returns a transducer function.  There are two arities
  of this function, one accepting a single reducer function and one that accepts
  a reducer function and an initial value.  Just like the transducer-producing
  arities in the standard library (e.g.: map, mapcat), this function does not
  include an arity accepting an input collection.

  Semantics of the transducer function reduce are the same as the semantics of a
  regular reduce."
  ([f initial-value]
   (fn [reducing-fn]
     (let [prev (volatile! initial-value)]
       (fn
         ([] (reducing-fn))
         ([result] (if-not (= @prev ::none)
                     (let [final-result (reducing-fn result @prev)]
                         (vreset! prev ::none)
                         final-result)
                     (reducing-fn result)))
         ([result input]
          (let [prior @prev]
            (if (= prior ::none)
              (do (vreset! prev input)
                  result)
              (let [next (f prior input)]
                (if (reduced? next)
                  (do (vreset! prev ::none)
                      (reducing-fn result @next))
                  (do (vreset! prev next)
                      result))))))))))
  ([f]
   (xreduce f ::none)))


(defn <-f
  "Translate an arbitrary function f into a transducing function that can be
  composed into a transducer chain using (comp f1 f2 f3 ... fn).

  For creating a reduce transducer with an initial value, pass a vector with
  the reducing function in the initial element and the initial value in the
  second element."
  [f]
  (cond
    (vector? f)     (do (must-be "Reducer function vectors must have 2 elements" (= 2 (count f)))
                        (xreduce (first f) (second f)))
    (= (arity f) 1) (xmapcat f)
    (= (arity f) 2) (xreduce f)
    :else           (throw (IllegalArgumentException. (str "Unexpected arity: " (arity f))))))


(defn <-fns
  "Compute a transducer function composing fns according to Unix pipe semantics
  as defined in <-f."
  [& fns]
  (let [transducers (map <-f (flatten fns))]
    (apply comp transducers)))
