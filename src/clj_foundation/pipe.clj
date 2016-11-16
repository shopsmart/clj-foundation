(ns clj-foundation.pipe
  "A Unix pipe function for Clojure that is implemented by converting regular Clojure
  functions into transducers over the input.

  Input can be any container or a simple type.  For a container, the output will
  be the same type as the input.  If the input is a simple type, it will be wrapped
  in a vector before being processed and the result will be a vector.  Supported
  container types include Map, List, Vector, and Set.

  fns may include any of the following:

  * An arity 1 function is treated as a mapcat function with the following relaxed
  rules:  If it returns a value of a simple type, the value is appended to the output.
  A nil result is considered an empty result.  Otherwise, mapcat semantics are followed.
  e.g.: if you want the result to be a collection of collections, the sub-collection
  must first be wrapped in another collection so the sub-collection itself will be
  concatinated onto the result.

  * An arity 2 function is treated as a reducing function where the initial two
  collection elements specify the initial two elements in the reduction.  The reducer
  supports the 'reduced' function in the standard library so that a single input
  collection can produce multiple reduced outputs.

  * A vector containing an arity 2 function and a second value treats the function as
  a reducer and the second value as the initial value in the reduction.

  The input is processed through fns, a single element at a time, without creating
  intermediate collections, in the order in which fns are specified."
(:require [clj-foundation.transducers :as transducer]))


(defn |
  "Unix pipeline semantics for Clojure.  Input can be any container or a simple type.
  For a container, the output will be the same type as the input.  If the input is a
  simple type, it will be wrapped in a vector before being processed and the result
  will be a vector.  Supported container types include Map, List, Vector, and Set.

  fns may include any of the following:

  * An arity 1 function is treated as a mapcat function with the following relaxed
  rules:  If it returns a value of a simple type, the value is appended to the output.
  A nil result is considered an empty result.  Otherwise, mapcat semantics are followed.
  e.g.: if you want the result to be a collection of collections, the sub-collection
  must first be wrapped in another collection so the sub-collection itself will be
  concatinated onto the result.

  * An arity 2 function is treated as a reducing function where the initial two
  collection elements specify the initial two elements in the reduction.  The reducer
  supports the 'reduced' function in the standard library so that a single input
  collection can produce multiple reduced outputs.

  * A vector containing an arity 2 function and a second value treats the function as
  a reducer and the second value as the initial value in the reduction.

  The input is processed through fns, a single element at a time, without creating
  intermediate collections, in the order in which fns are specified."
 [input & fns]
 (let [input (if (or (vector? input) (sequential? input)) input [input])
       composed-fns (transducer/<-fns fns)]
   (sequence composed-fns input)))
