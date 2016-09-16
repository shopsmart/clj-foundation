(ns clj-foundation.errors-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.errors :refer :all]
            [clj-foundation.data :refer [any?]]
            [clj-foundation.patterns :as p]
            [clj-foundation.millis :as millis]))


(common/register-fixtures)

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



(deftest retry?-test
  (testing "Retry up to :max-retries times"
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
      (is (= :ABORT-MAX-RETRIES (retry? r3 TIMEOUT-ERROR)))))

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
  (testing "Success -> Results!!!"
    (is (= "Results!!!"
           (retry-with-timeout
            "Success"
            (->RetrySettings 1 (millis/<-seconds 1) (millis/<-seconds 5) (constantly false))
            (constantly "Results!!!")))))

  (testing "Taking too much time fails!"
    (is (instance? RuntimeException
                   (try*
                    (retry-with-timeout
                     "Sloooow"
                     (->RetrySettings 3 (millis/<-seconds 1) 50 (constantly false))
                     #(Thread/sleep (millis/<-seconds 2)))))))

  (testing "Fatal errors abort retrying"
    (is (instance? RuntimeException
                   (try*
                    (retry-with-timeout
                     "Ooops..."
                     (->RetrySettings 3 (millis/<-seconds 1) (millis/<-seconds 5) (constantly true))
                     #(throw (RuntimeException.)))))))

  (testing "If at first you don't succeed, try, try again..."
    (let [attempts (atom 0)
          job-fn (fn []
                   (swap! attempts inc)
                   (when (< @attempts 2) (throw (RuntimeException. "Not this time")))
                   "Finally--success!")]
      (is (= "Finally--success!"
             (retry-with-timeout
              "Persistance pays off"
              (->RetrySettings 3 (millis/<-seconds 1) (millis/<-seconds 5) (constantly false))
              job-fn)))
      (is (= 2 @attempts)))))


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
