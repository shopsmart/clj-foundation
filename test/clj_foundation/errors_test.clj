(ns clj-foundation.errors-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.errors :refer :all]))



(deftest failure?--predefined-failure-modes
  (testing "Nil is not a failure"
    (is (not (failure? nil))))

  (testing "Exceptions are failures"
    (is (failure? (IllegalStateException.)))
    (is (failure? (Throwable.))))

  (testing "Timeouts are failures"
    (is (failure? TIMEOUT-ERROR)))

  (testing "Other objects are not failures by default"
    (is (not (failure? "Woo hoo!")))
    (is (not (failure? 2/3)))
    (is (not (failure? 3.141592)))
    (is (not (failure? (Object.))))))


(deftest try*-with-timeout-test
  (testing "Success returns results"
    (is (= 42 (try*-with-timeout 1000 (fn [x] x) 42))))

  (testing "Thrown exceptions are returned"
    (is (instance? RuntimeException (try*-with-timeout 1000 (fn [] (throw (RuntimeException.)))))))

  (testing "TIMEOUT-ERROR is returned on timeout"
    (is (= TIMEOUT-ERROR (try*-with-timeout 500 (fn [] (Thread/sleep 1000)))))))


(deftest retry-with-timeout-test
  (testing ""))


(run-tests)

;; Necessary when testing multimethods:
(ns-unmap *ns* 'failure?)
