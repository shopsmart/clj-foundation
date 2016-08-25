(ns clj-foundation.templates-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.templates :refer :all])
  (:import [clojure.lang ExceptionInfo]))

(s/set-compile-fn-validation! true)


(deftest subst<-test
  (testing "All variables are substituted"
    (is (= "Hello, world: It's time to party!" (subst<- "Hello, ${name}: ${message}" :name "world" :message "It's time to party!"))))

  (testing "If a variable isn't specified, throws"
    (is (thrown? ExceptionInfo (subst<- "Hello, ${name}: ${message}" :name "world")))))


(deftest parameters<-test
  (testing "Returns template varibles specified in template in order"
    (is (= [:foo :bar :foo :baz] (parameters<- "${foo} ${bar} ${foo} some stuff ${baz}")))))


(deftest parameter-list<-test
  (testing "Converts template string into resolved parameter list"
    (is (= ["one" "two"] (parameter-list<- "${foo} to ${bar}" {:foo "one" :bar "two"}))))

  (testing "Converts parameter name vector into resolved parameter list"
    (is (= ["one" "two"] (parameter-list<- [:foo :bar] {:foo "one" :bar "two"}))))

  (testing "If string parameters cannot be resolved, throws ExceptionInfo unless :partial-resolve true is specified."
    (is (thrown? ExceptionInfo (vec (parameter-list<- "${foo} to ${bar}" {:foo "two"}))))
    (is (= ["two" "${bar}"] (parameter-list<- "${foo} to ${bar}" {:foo "two"} :partial-resolve true))))

  (testing "If vector parameters cannot be resolved, throws ExceptionInfo unless :partial-resolve true is specified."
    (is (thrown? ExceptionInfo (vec (parameter-list<- [:foo :bar] {:foo "two"}))))
    (is (= ["two" "${bar}"] (parameter-list<- [:foo :bar] {:foo "two"} :partial-resolve true)))))


(run-tests)
