(ns clj-foundation.errors
  (:require [clojure.tools.logging :as log]
            [schema.core :as s :refer [=> =>*]])

  (:import [java.util Date])

  (:gen-class))


(defn replace-nil
  [maybe-nil replacement]
  (if (nil? maybe-nil)
    replacement
    maybe-nil))


(defn not-nil [value name]
  (if (nil? value)
    (throw (java.lang.IllegalArgumentException. (str name " cannot be nil")))
    value))


(defprotocol ComputationFailed
  "A protocol that determines if a computation has resulted in a failure.
   This allows the definition of what constitutes a failure to be extended
   to new types by the consumer."
  (failure? [self]))

(extend-protocol ComputationFailed
  Object
  (failure? [self] false)

  Throwable
  (failure? [self] true)

  nil
  (failure? [self] false))


(s/defn expect-within :- s/Any
  "Expect the condition specified by predicate to become true within timeout-millis. If this
  does not happen, throws IllegalStateException including the error-message.  On success, returns
  the truthy value that predicate returned."
  [timeout-millis :- s/Num, predicate :- (=> s/Any []), error-message :- s/Str]

  (let [before (.getTime (Date.))]
    (loop [completed (predicate)]
      (let [later (.getTime (Date.))]
        (if (< timeout-millis (- later before))
          (throw (IllegalStateException. error-message))
          (if-not completed
            (do
              (Thread/sleep 500)
              (recur (predicate)))
            completed))))))


(defmacro try*
  "A variant of try that translates exceptions into return values or a
  specified default value"
   ([body]
    `(try ~body (catch Throwable e# e#)))
   ([body default-value-if-failure]
    `(try ~body (catch Throwable e# ~default-value-if-failure))))


(defn retry*
  "Retry calling the specified function f & args while pausing pause-millis
between attempts.  Throwable objects, and uncaught exceptions are all
considered errors.  After tries attempts, the last error is returned."
  [tries pause-millis f & args]
  (let [res (try* (apply f args))]
    (if (not (failure? res))
      res
      (if (= 0 tries)
        res
        (do
          (if (instance? Throwable res)
            (log/error res "A failure occurred; retrying...")
            (log/error (str "A failure occurred; retrying...  [" (pr-str res) "]")))
          (Thread/sleep pause-millis)
          (recur (dec tries) pause-millis f args))))))

(defn retry
  "Retry calling the specified function f & args while pausing pause-millis
between attempts.  Uncaught exceptions are considered errors.  After tries
attempts, the last caught exception is re-thrown."
  [tries pause-millis f & args]
  (let [res (try {:value (apply f args)}
                 (catch Exception e
                   (if (= 0 tries)
                     (throw e)
                     {:exception e})))]
    (if (:exception res)
      (do
        (log/error (:exception res) "A failure occurred; retrying...")
        (Thread/sleep pause-millis)
        (recur (dec tries) pause-millis f args))
      (:value res))))


(defn retry-statements
  "Specify a retry policy for a function that will be called later.  tries is the
  number of retries allowed for the future function call, and pause-millis is the
  number of milliseconds to wait between retries.

  Returns a multi-arity function.  The initial parameter is the function to be called.
  The remaining parameters (if any) are that function's arguments."
  [tries pause-millis]
  (fn [f & args] (apply retry tries pause-millis f args)))
