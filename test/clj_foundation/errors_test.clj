(ns clj-foundation.errors-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.io :as io :refer [with-err-str]]
            [clj-foundation.errors :refer :all]
            [clj-foundation.data :refer [any?]]
            [clj-foundation.patterns :as p]
            [clj-foundation.millis :as millis]
            [clj-foundation.errors :as err]
            [clojure.string :as str])
  (:import [java.util Date]
           [clojure.lang ExceptionInfo]))


(common/register-fixtures)


(deftest trace-test
  (let [result (with-out-str (trace "testing 123"))]
    (testing "result contains ns, line, and column of trace message"
      (is (.contains (str *ns*) result))
      (is (.contains ":line" result))
      (is (.contains ":column" result)))))


(deftest failure?--predefined-failure-modes-test
  (testing "Nil is not a failure"
    (is (not (failure? nil))))

  (testing "Exceptions are failures"
    (is (failure? (IllegalStateException.)))
    (is (failure? (Throwable.))))

  (testing "Timeouts are failures"
    (is (failure? TIMEOUT-ERROR)))

  (testing "The error value is a failure"
    (is (failure? p/NO-RESULT-ERROR)))

  (testing "Other objects are not failures by default"
    (is (not (failure? "Woo hoo!")))
    (is (not (failure? 2/3)))
    (is (not (failure? :error)))        ; Needs an explicit (defmethod failure?...
    (is (not (failure? 3.141592)))
    (is (not (failure? (Object.))))))


(let [e1    (Exception. "Just one")

      e2'   (Exception. "I'm guilty.")
      e2    (Exception. "An exception with a cause." e2')

      e3''  (Exception. "only table or database owner can vacuum it")
      e3'   (Exception. "Look deeper" e3'')
      e3    (Exception. "An exception with a fatal cause." e3')

      e4''' (Exception. "Lotssss offff causses, my Preciousss!")
      e4''  (Exception. "only table or database owner can vacuum it" e4''')
      e4'   (Exception. "Look deeper" e4'')
      e4    (Exception. "An exception with a fatal cause." e4')

      e5    (ex-info "Master exception" {} e4)]

  (deftest seq<-test
    (testing "One exception with no cause returns a list of that exception only"
      (is (= "Just one" (.getMessage (first (seq<- e1)))))
      (is (= 1 (count (seq<- e1)))))

    (testing "Nested exceptions add items to the exception list"
      (is (= 2 (count (seq<- e2))))
      (is (= 3 (count (seq<- e3)))))

    (testing "ex-info :via maps are parsed"
      (is (= 5 (count (seq<- e5)))))))


(deftest metalog-test
  (testing "Warnings and below print to *out*"
    (is (= "warning\n" (with-out-str
                         (log :warn "warning")))))

  (testing "Errors and above print to *err*"
    (is (= "error\n"   (with-err-str
                         (log :error "error")))))

  (testing "Can replace the metalogger globally"
    (set-global-metalogger
     (fn [level & more]
       (apply metalog level "OVERRIDDEN:" more)))

    (is (str/starts-with? (with-out-str
                            (log :warn "Three Blind Locusts"))
                          "OVERRIDDEN:"))

    (set-global-metalogger metalog)))


(deftest expect-within-test
  (testing "Immediate success returns immediately"
    (let [before  (.getTime (Date.))
          success (expect-within (millis/<-seconds 2)
                                 (constantly true)
                                 "Always succeeds immediately")
          after   (.getTime (Date.))
          time    (- after before)]

      (is success)
      (is (< time (millis/<-seconds 1/4)))))

  (testing "Eventual success succeeds"
    (let [tries   (atom 0)
          before  (.getTime (Date.))
          success (expect-within (millis/<-seconds 5)
                                 (fn [] (swap! tries inc) (< 3 @tries))
                                 "Always succeeds eventually")
          after   (.getTime (Date.))
          time    (- after before)]

      (is success)
      (is (< time (millis/<-seconds 4)))))

  (testing "Taking too much time throws IllegalStateException"
    (let [timeout (millis/<-seconds 1)
          before  (.getTime (Date.))
          result  (try* (expect-within timeout
                                       (constantly false)
                                       "Never succeeds"))
          after   (.getTime (Date.))
          time    (- after before)]

     (is (instance? IllegalStateException result))
     (is (< timeout time)))))


(deftest retry?-test
  (testing "Retry up to :max-retries times: "
    (testing "too many timeouts"
      (let [r0 (new-default-job "timeout-test"
                                3
                                (millis/<-seconds 0.25)
                                (constantly false))
            r1 (update-in r0 [:retries] inc)
            r2 (update-in r1 [:retries] inc)
            r3 (update-in r2 [:retries] inc)]

        (is (= :RETRY-TIMEOUT     (retry? r0 TIMEOUT-ERROR)))
        (is (= :RETRY-TIMEOUT     (retry? r1 TIMEOUT-ERROR)))
        (is (= :RETRY-TIMEOUT     (retry? r2 TIMEOUT-ERROR)))
        (is (= :ABORT-MAX-RETRIES (retry? r3 TIMEOUT-ERROR))))

      (testing "too many errors"
        (let [r0 (new-default-job "Too man errors test"
                                  3
                                  (millis/<-seconds 0.25)
                                  (constantly false))
              r1 (update-in r0 [:retries] inc)
              r2 (update-in r1 [:retries] inc)
              r3 (update-in r2 [:retries] inc)
              e  (Exception. "Something bad happened!")]

          (is (= :RETRY-FAILURE     (retry? r0 e)))
          (is (= :RETRY-FAILURE     (retry? r1 e)))
          (is (= :RETRY-FAILURE     (retry? r2 e)))
          (is (= :ABORT-MAX-RETRIES (retry? r3 e)))))))


  (testing "Abort on fatal errors"
    (let [r0 (new-default-job "abort-test"
                              4
                              (millis/<-seconds 1)
                              #(any? (fn [x] (instance? Throwable x)) %))
          r1 (update-in r0 [:retries] inc)
          r2 (update-in r1 [:retries] inc)
          r3 (update-in r2 [:retries] inc)]

      (is (= :RETRY-TIMEOUT     (retry? r0 TIMEOUT-ERROR)))
      (is (= :RETRY-TIMEOUT     (retry? r1 TIMEOUT-ERROR)))
      (is (= :ABORT-FATAL-ERROR (retry? r2 (Exception.))))))

  (testing "Retry non-fatal errors"
    (let [r0 (new-default-job "successful-retry"
                              4
                              (millis/<-seconds 1)
                              (constantly false))]
      (is (= :RETRY-FAILURE (retry? r0 (Exception.)))))))


(deftest try*-timeout-millis-test
  (testing "Success returns results"
    (is (= 42 (try*-timeout-millis 1000 42))))

  (testing "Thrown exceptions are returned"
    (is (instance? RuntimeException (try*-timeout-millis 1000 (throw (RuntimeException.))))))

  (testing "TIMEOUT-ERROR is returned on timeout"
    (is (= TIMEOUT-ERROR (try*-timeout-millis 500 (Thread/sleep 1000))))))


(deftest retry-with-timeout-test
  (testing "nil arguments throw ExceptionInfo"
    (testing "nil in main argument list"
      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    nil
                    (->RetrySettings 1 (millis/<-seconds 1) (millis/<-seconds 5) (constantly false))
                    (constantly "nil not allowed!"))))

      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    "nil throws!"
                    nil
                    (constantly "nil not allowed!"))))

      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    "nil throws!"
                    (->RetrySettings 1 (millis/<-seconds 1) (millis/<-seconds 5) (constantly false))
                    nil))))

    (testing "nil in RetrySettings"
      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    "nil throws!"
                    (->RetrySettings nil (millis/<-seconds 1) (millis/<-seconds 5) (constantly false))
                    (constantly "nil not allowed!"))))

      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    "nil throws!"
                    (->RetrySettings 1 nil (millis/<-seconds 5) (constantly false))
                    (constantly "nil not allowed!"))))

      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    "nil throws!"
                    (->RetrySettings 1 (millis/<-seconds 1) nil (constantly false))
                    (constantly "nil not allowed!"))))

      (is (thrown? ExceptionInfo
                   (retry-with-timeout
                    "nil throws!"
                    (->RetrySettings 1 (millis/<-seconds 1) (millis/<-seconds 5) nil)
                    (constantly "nil not allowed!"))))))

  (testing "Happy paths: "
    (testing "Success -> Results!!!"
      (is (= "Results!!!"
             (retry-with-timeout
              "Success"
              (->RetrySettings 1 (millis/<-seconds 1) (millis/<-seconds 5) (constantly false))
              (constantly "Results!!!")))))

    (testing "If at first you don't succeed, try, try again...: "
      (testing "With failures."
        (let [attempts              (atom 0)
              start                 (System/currentTimeMillis)
              timeout-time          (millis/<-seconds 1)
              pause-time            (millis/<-seconds 5)
              expected-elapsed-time pause-time
              job-fn                (fn []
                                      (swap! attempts inc)
                                      (when (< @attempts 2) (throw (RuntimeException. "Not this time")))
                                      "Finally--success!")]
          (is (= "Finally--success!"
                 (retry-with-timeout
                  "Persistance pays off"
                  (->RetrySettings 3 timeout-time pause-time (constantly false))
                  job-fn)))
          (is (= 2 @attempts))
          (is (<= expected-elapsed-time (- (System/currentTimeMillis) start)))))

      (testing "with timeouts."
        (let [attempts              (atom 0)
              start                 (System/currentTimeMillis)
              timeout-time          (millis/<-seconds 1)
              pause-time            (millis/<-seconds 5)
              expected-elapsed-time (+ timeout-time pause-time)
              job-fn                (fn []
                                      (swap! attempts inc)
                                      (when (< @attempts 2) (Thread/sleep (+ timeout-time 500)))
                                      "Finally--success!")]
          (is (= "Finally--success!"
                 (retry-with-timeout
                  "Persistance pays off"
                  (->RetrySettings 3 timeout-time pause-time (constantly false))
                  job-fn)))
          (is (= 2 @attempts))
          (is (<= expected-elapsed-time (- (System/currentTimeMillis) start)))))))

  (testing "Sad paths: "
    (testing "Taking too much time fails when abort?-fn is (constantly false)!"
      (is (thrown? RuntimeException
                   (retry-with-timeout
                    "Sloooow"
                    (->RetrySettings 3 (millis/<-seconds 1) 50 (constantly false))
                    #(Thread/sleep (millis/<-seconds 2))))))

    (testing "Taking too much time fails--even with retries! (Note: timeouts are not exceptions so abort?-fn is not relevant)"
      (let [total-tries (atom 0)]
        (is (thrown? RuntimeException
                     (retry-with-timeout
                      "Sloooow"
                      (->RetrySettings 3 (millis/<-seconds 1) 50 (constantly true))
                      (fn []
                        (swap! total-tries inc)
                        (Thread/sleep (millis/<-seconds 2))))))
        (is (= 4 @total-tries))))

    (testing "Too many retries fails"
      (let [total-tries (atom 0)]
        (is (thrown? Exception
                     (retry-with-timeout
                      "Boom"
                      (->RetrySettings 3 (millis/<-seconds 1) 50 (constantly false))
                      (fn []
                        (swap! total-tries inc)
                        (throw (Exception.))))))
        (is (= 4 @total-tries))))

    (testing "Fatal errors abort retrying"
      (let [total-tries (atom 0)]
        (is (thrown? Exception
                     (retry-with-timeout
                      "Ooops..."
                      (->RetrySettings 3 (millis/<-seconds 1) (millis/<-seconds 5) (constantly true))
                      (fn []
                        (swap! total-tries inc)
                        (throw (Exception.))))))
        (is (= 1 @total-tries))))))


;; (dis)allowed values ------------------------------------------------------------------

(deftest must-be-test
  (let [foo 1
        blatz 42
        bar 42]

    (testing "must-be throws IllegalArgumentException when body is falsey and returns result of evaluating body otherwise"
      (is (instance? IllegalArgumentException
                     (try* (must-be "Expected foo to equal blatz"
                                    (= foo blatz)))))

      (is (instance? IllegalArgumentException
                     (try* (must-be "Expected non-nil"
                                    nil))))

      (is (= 42 (must-be "Meaning of life" 42))))

    (testing "Must-be returns last value in a compound body"
      (is (= 1 (must-be "One" 42 1)))

      (is (instance? IllegalArgumentException
                     (try* (must-be "Truthy" blatz (println blatz))))))))



(run-tests)
