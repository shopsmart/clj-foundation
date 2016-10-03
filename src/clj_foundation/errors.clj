(ns clj-foundation.errors
  (:require [clojure.tools.logging :as log]
            [clj-foundation.patterns :refer :all]
            [clj-foundation.millis :as millis]
            [schema.core :as s :refer [=> =>*]])

  (:import [java.util Date])

  (:gen-class))


;; Traceability ---------------------------------------------------------------------

(defmacro trace
  "Like str but prepends the namespace and line/column of the call site."
  [& more]
  (let [line-col (vec (meta &form))
        s        (apply str *ns* line-col " " more)]
    `~s))


;; Extensible failure objects / test multimethod -------------------------------------


(defmulti failure?
  "A multimethod that determines if a computation has resulted in a failure.
  This allows the definition of what constitutes a failure to be extended
  to new types by the consumer.

  An example of how this can function can be extended to new error types
  exists in this namespace where we extend failure? to include timeout errors."
  (fn [val] [(type val) val]))


(defmethod failure? [nil nil]
  [_]
  "Nil is not a failure."
  false)


(defmethod failure? [(Nothing!) NO-RESULT-ERROR]
  [_]
  "The 'error' value of the Nothing type is a failure."
  true)


(defmethod failure? :default
  [val]
  "Ordinary objects are only failures if they are Throwable."
  (instance? Throwable val))


(s/defn exception<- :- Throwable
  "If x is already Throwable return it, else convert it into an exception using ex-info.  The
  (:error-object result) will be the original value.  This is intended--though not strictly
  required--to be used for values where (failure? value) is true."
  [x :- s/Any]
  (cond
    (instance? Throwable x) x
    :else                   (ex-info (str x) {:error-obj x})))


(s/defn seq<- :- [Throwable]
  "Converts failures into seqs of exceptions.  If the failure is already an exception (the common case),
  it returns a seq starting with the root exception, and recursively including (.getCause e)
  until there are no more causes.

  If the failure is a seq, ensures that the result is a seq of excetions.

  If the failure isn't already an exception or a seq, it is converted into one first using ex-info.  In this case,
  the :cause in the ex-info map will be the original failure object."
  [failure :- (s/pred failure? "(failure? failure) is truthy")]
  (cond
    (seq? failure)                (map exception<- failure)
    (instance? Throwable failure) (lazy-seq (cons failure (seq<- (.getCause failure))))
    (not (nil? failure))          (seq<- (exception<- failure))))


(def exception-seq
  "Deprecated.  Use errors/seq<- instead."
  seq<-)


(s/defn stack-trace<- :- s/Str
  "Returns the stack trace associated with e as a String."
  [e :- Throwable]
  (let [output (java.io.ByteArrayOutputStream.)
        printer (java.io.PrintStream. output)]
    (.printStackTrace e printer)
    (.toString output)))


(defmacro try*
  "A variant of try that translates exceptions into return values or a
  specified default value.  Note that body must be a single statement.
  If you need more than that, then wrap your statement inside a \"do\". "
  ([body]
   `(try ~body (catch Throwable e# e#)))
  ([body default-value-if-failure]
   `(try ~body (catch Throwable e# ~default-value-if-failure))))


;; Various retry/timeout strategies ---------------------------------------------------

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


(defn retry*
  "Retry calling the specified function f & args while pausing pause-millis
  between attempts.  Throwable objects, and uncaught exceptions are all
  considered errors.  After tries attempts, the last error is returned."
  [tries pause-millis f & args]
  (let [res (try* (apply f args))]
    (if-not (failure? res)
      res
      (if (zero? tries)
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
                   (if (zero? tries)
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


(defmethod failure? [clojure.lang.Keyword TIMEOUT-ERROR] [_]
  "Timeout-errors are failures."
  true)


(defn timeout?
  "True if (= TIMEOUT-ERROR e)"
  [e]
  (= TIMEOUT-ERROR e))


(defmacro try*-timeout-millis
  "Execute body with a specified timeout inside a try* block so that thrown exceptions
  are returned.

  On success, returns the result of executing body.  On failure, returns either the
  failure exception or TIMEOUT-ERROR on timeout."
  [timeout-millis & body]
  `(deref (future (try* (do ~@body)))
          ~timeout-millis
          TIMEOUT-ERROR))


(s/defn retry? :- (s/enum :ABORT-MAX-RETRIES :ABORT-FATAL-ERROR :RETRY-FAILURE :RETRY-TIMEOUT)
  "Something failed.  Examine the retry count and exact failure cause and determine if we can
  retry the operation.  Internal API only; public so we can document using Schema and test."
  [job                         ; Conforms to map returned by new-default-job
   failure-value :- s/Any]
  (let [job-abort?       (:abort?-fn job)
        result-exception (exception<- failure-value)]
    (cond
      (timeout? failure-value)              (if (< (:retries job) (:max-retries job))
                                              :RETRY-TIMEOUT
                                              :ABORT-MAX-RETRIES)

      (job-abort? (seq<- result-exception))   :ABORT-FATAL-ERROR
      :else                                   :RETRY-FAILURE)))


(defn new-default-job
  "Create a Job object.  Only public to make retry? testable."
  [job-name tries pause-millis abort?-fn]

  {:job-name job-name
   :abort?-fn abort?-fn
   :retries 0
   :max-retries tries
   :retry-pause-millis pause-millis})


(s/defrecord RetrySettings
    [tries          :- s/Num
     timeout-millis :- s/Num
     pause-millis   :- s/Num
     abort?-fn      :- (=> s/Bool [[Throwable]])])


(s/defn retry-with-timeout :- s/Any
  "Retry (apply f args) up to tries times with pause-millis time in between invocation and a
  timeout value of timeout-millis.  On failure, abort?-fn is called with a vector containing the
  unwrapped exception stack.

  (failure is determined via the (failure? x) multimethod so clients can extend the set of values
  that are considered to be failures.)

  If abort?-fn returns true, the errror is considered fatal and no more retries are attempted,
  even if retries were available.

  If the last result is a failure, and that failure is Throwable, the exception is wrapped in a
  RuntimeException and rethrown.

  If the last result is a TIMEOUT-ERROR, a runtime exception is thrown.  Otherwise, the failure
  value itself is returned as the result."

  [job-name       :- String
   settings       :- RetrySettings
   f              :- (=> s/Any [s/Any])
   & args         :- [s/Any]]

  (let [tries          (:tries settings)
        timeout-millis (:timeout-millis settings)
        pause-millis   (:pause-millis settings)
        abort?-fn      (:abort?-fn settings)]
    (loop [j (new-default-job job-name tries pause-millis abort?-fn)]
      (let [result (try*-timeout-millis timeout-millis (apply f args))]
        (if (failure? result)
          (do
            (case (retry? j result)
              :ABORT-MAX-RETRIES (throw     (RuntimeException. (str "MAX-RETRIES(" tries ")[" job-name "]: " (.getMessage result)) result))
              :ABORT-FATAL-ERROR (throw     (RuntimeException. (str "FATAL[" job-name "]: " (.getMessage result)) result))
              :RETRY-FAILURE     (log/error result (str "RETRY[" job-name "]; " (type result) ": " (.getMessage result)))
              :RETRY-TIMEOUT     (log/error (RuntimeException. "Timeout.") (str "RETRY[" job-name "]: Took longer than " timeout-millis " ms."))
              :else              (throw     (IllegalStateException. "Program Error!  We should never get here.")))
            (recur (update-in j [:retries] inc)))
          result)))))


;; (dis)allowed values ------------------------------------------------------------------


(s/defn not-nil :- s/Any
  "If value is not nil, returns it, else throws IllegalArgumentExceoption with
  the message \"${name} cannot be nil\""
  [value :- s/Any, name :- s/Str]
  (if (nil? value)
    (throw (java.lang.IllegalArgumentException. (str name " cannot be nil")))
    value))


(s/defn not-failure :- s/Any
  "If value is not a failure, returns it, else throws IllegalStateExceoption with
  the specified message"
  [value :- s/Any, message :- s/Str]
  (if (nil? value)
    (if (instance? Throwable value)
      (throw (java.lang.IllegalStateException. message value))
      (throw (java.lang.IllegalStateException. (str "[" value "]: " message))))
    value))


(s/defn throw-or :- s/Any
  "If value is a failure, wrap and throw it in an IllegalStateException
  with the specified message, else run function on the value and return the
  result"
  [value :- s/Any, message :- s/Str, f :- (=> s/Any [s/Any])]
  (cond
    (failure? value) (throw (IllegalStateException. message value))
    :else            (f value)))


(defmacro must-be
  "If body is truthy returns result of evaluating body, else throws IllegalArgumentException with message."
  [message & body]
  (let [line-col (vec (meta &form))
        ns       *ns*]
    `(let [result# (do ~@body)]
       (if result#
         result#
         (throw (IllegalArgumentException. (str ~ns ~line-col " " ~message)))))))
