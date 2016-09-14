(ns clj-foundation.filters
  (:require [clj-foundation.patterns :as p]
            [clj-foundation.errors :as err]
            [schema.core :as s :refer [=> =>*]]
            [clojure.string :as str]
            [potemkin :refer [def-map-type]])
  (:gen-class))


(defn any?
  "Returns coll if any element in coll satisfies predicate."
  [predicate coll]
  (if (not-empty (filter predicate coll))
    coll
    nil))


(s/defn replace-if :- s/Any
  "In the binary form, replace value with the result of running predicate against value
   if that result is truthy.  In the terniary form, replace value with replacement if
   the result of running predicate against value is truthy.

   An idiomatic way to use this function is to pass a set of disallowed values as
   predicate, and a replacement value as replacement."

  ([value :- s/Any, predicate :- (=> s/Any [s/Any])]
   (let [maybe-result (predicate value)]
     (if maybe-result
       maybe-result
       value)))

  ([value :- s/Any, predicate :- (=> s/Any [s/Any]), replacement :- s/Any]
   (if (predicate value)
     value
     replacement)))


(s/defn replace-nil :- s/Any
  "Accepts a value that cannot be nil; if it is not nil it returns it, else it
  returns its replacement."
  [maybe-nil :- s/Any, replacement :- s/Any]
  (replace-if maybe-nil #{nil} replacement))


(s/defn nothing->identity :- s/Any
  "Takes nil or Nothing to the specified identity value for the type and computation in context,
  otherwise returns value.  An identity value can be applied to a value of the given type under the
  operation in context without affecting the result.  For example 0 is the identity value for rational
  numbers under addition.  The empty string is the identity value for strings under concatination.

  Note that Nothing is already an identity value for maps and seqs.  This function is only useful
  for types where the Nothing type is ill-behaved (e.g.: Strings, Numbers, ...) for a given operation.

  Another name for this concept is the monadic zero for the type/operation."
  [identity-value :- s/Any, value :- s/Any]

  (if (p/something? value)
    value
    identity-value))


(s/defn identity->nil :- s/Any
  "Synopsis:
     (identity->nil [])                                  --> nil
     (identity->nil \"\")                                --> nil
     (identity->nil 1 #{1})                              --> nil   ; e.g.: Under multiplication
     (identity->nil \"none\" #{\"nil\" \"none\" \" \"})  --> nil

  If value is empty (for its type's natural definition of empty), returns nil.  Otherwise returns
  value.

  * Non-numeric values are empty iff (empty? value).
  * Numbers default to zero as their identity value.
  * The identity predicate may optionally be overridden in the second parameter."
  ([value      :- s/Any
    identity-p :- (=> s/Bool [s/Any])]
   (if (identity-p value) nil value))

  ([value      :- s/Any]
   (cond
     (number? value) (identity->nil value zero?)
     :else           (identity->nil value empty?))))


;; FIXME: The following two do roughly the same thing.  Consolidate.

(s/defn value-or :- s/Any
  "If value is nil or an instance of Nothing, run f and return its result.  Else, return value."
  [value :- s/Any, f :- (=> s/Any [s/Any])]
  (if (or (nil? value)
          (instance? (p/Nothing!) value))
    (f value)
    value))


(s/defn something-or :- s/Any
  "If value is not Nothing return value, else run f and return its result."
  [value :- s/Any, f :- (=> s/Any [s/Any])]
  (if (p/something? value)
    value
    (f value)))
