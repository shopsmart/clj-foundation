(ns clj-foundation.errors-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.errors :refer :all]
            [clj-foundation.patterns :as p :refer [any?]]
            [clj-foundation.millis :as millis]))



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
            1
            (millis/<-seconds 1)
            (millis/<-seconds 5)
            (constantly false)
            (constantly "Results!!!")))))

  (testing "Taking too much time fails!"
    (is (instance? RuntimeException
                   (try*
                    (retry-with-timeout
                     "Sloooow"
                     3
                     (millis/<-seconds 1)
                     50
                     (constantly false)
                     #(Thread/sleep (millis/<-seconds 2)))))))

  (testing "Fatal errors abort retrying"
    (is (instance? RuntimeException
                   (try*
                    (retry-with-timeout
                     "Ooops..."
                     3
                     (millis/<-seconds 1)
                     50
                     (constantly true)
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
              3
              (millis/<-seconds 1)
              (millis/<-seconds 5)
              (constantly false)
              job-fn)))
      (is (= 2 @attempts)))))


(run-tests)

;; Necessary when testing multimethods:
(ns-unmap *ns* 'failure?)
