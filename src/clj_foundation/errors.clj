(ns clj-foundation.errors
  (:require [clojure.tools.logging :as log]
            [clj-foundation.patterns :refer :all]
            [schema.core :as s :refer [=> =>*]])

  (:import [java.util Date])

  (:gen-class))


(s/defn exception-seq :- [Throwable]
  "Returns a lazy sequence of all nested exceptions starting with the top-level exception and working to
   the initial exception by recursively unrolling the (.getCause exception) for each nested exception.."
  [exception :- Throwable]
  (if exception
    (lazy-seq (cons exception (exception-seq (.getCause exception))))))


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



(ns-unmap *ns* 'failure?)               ; Keep the REPL happy on reload

(defmulti failure?
  "A multimethod that determines if a computation has resulted in a failure.
   This allows the definition of what constitutes a failure to be extended
   to new types by the consumer."
  (fn [val] [(type val) val]))

(defmethod failure? [nil nil] [_] false)

(defmethod failure? :default
  [val]
  (instance? Throwable val))



(defmacro try*
  "A variant of try that translates exceptions into return values or a
  specified default value.  Note that body must be a single statement.
  If you need more than that, then wrap your statement inside a \"do\". "
  ([body]
   `(try ~body (catch Throwable e# e#)))
  ([body default-value-if-failure]
   `(try ~body (catch Throwable e# ~default-value-if-failure))))


;; Various retry strategies -------------------------------------------------------

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


;; Retry calling fn with a specified timeout; abort early if abort? returns truthy --


(def TIMEOUT-ERROR
  "A timeout error value"
  ::TIMEOUT-ERROR)

(defmethod failure? [clojure.lang.Keyword TIMEOUT-ERROR] [_] true)


(s/defn retry? :- s/Bool
  "Private implementation detail for retry-with-timeout.  Schemas added purely for code
  clarity."
  [job :- Job
   res :- s/Any]
  (let [abort? (:abort? job)]
    (if (= res TIMEOUT-ERROR)
      (< (:retries job) (:max-retries job))
      (not (abort? (exception-seq res))))))


(defn try*-with-timeout
  "(apply f args) with a specified timeout.  On success, returns the result of calling f.
  On failure, returns either the exception or TIMEOUT-ERROR on timeout."
  [timeout-millis f & args]
  (deref (future (try* (apply f args)))
         timeout-millis
         TIMEOUT-ERROR))


(s/defn retry-with-timeout :- s/Any
  "Retry (apply f args) up to tries times with pause-millis time in between invocation and a
  timeout value of timeout-millis.  On failure, abort?-fn is called with the failure value.
  If abort?-fn returns true, the errror is considered fatal and no more retries are attempted,
  even if retries were available."
  [tries :- s/Num
   pause-millis :- s/Num
   timeout-millis :- s/Num
   abort?-fn :- (=> s/Bool [[Throwable]])
   f :- (=> s/Any [])
   & args :- []]
  (let [default-job {:abort? abort?-fn
                     :job-fn f
                     :timeout-millis timeout-millis
                     :retries 0
                     :max-retries tries
                     :retry-pause-millis pause-millis}]
    (loop [j default-job]
      (let [res (apply try*-with-timeout timeout-millis f args)]
        (if (failure? res)
          (if (retry? j res)
            (do
              (log/error (:exception res) "A failure occurred; retrying...")
              (Thread/sleep pause-millis)
              (recur (update-in j :retries inc)))
            (if (instance? Throwable res)
              (throw res)
              res))
          res)))))


;; Replace disallowed values ------------------------------------------------------------------


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


(s/defn not-nil :- s/Any
  "If value is not nil, returns it, else throws IllegalArgumentExceoption with
  the message \"${name} cannot be nil\""
  [value :- s/Any, name :- s/Str]
  (if (nil? value)
    (throw (java.lang.IllegalArgumentException. (str name " cannot be nil")))
    value))


(s/defn value-or :- s/Any
  "If value is nil or Nothing, run f and return its result.  Else, return value."
  [value :- s/Any, f :- (=> s/Any [s/Any])]
  (if (#{nil nothing} value)
    (f value)
    value))


(s/defn something-or :- s/Any
  "If value is not Nothing return value, else run f and return its result."
  [value :- s/Any, f :- (=> s/Any [s/Any])]
  (if (something? value)
    value
    (f value)))


(s/defn throw-or :- s/Any
  "If value is a failure, wrap and throw it in an IllegalStateException
  with the specified message, else run function on the value and return the
  result"
  [value :- s/Any, message :- s/Str, f :- (=> s/Any [s/Any])]
  (cond
    (failure? value) (throw (IllegalStateException. message value))
    :else            (f value)))
