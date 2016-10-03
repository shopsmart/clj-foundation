(ns clj-foundation.data-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.data :refer :all]))


(common/register-fixtures)

(deftest identity->nil-test
  (testing "Numbers use zero? as their default identity predicate"
    (is (nil?                (identity->nil 0)))
    (is (= 42                (identity->nil 42))))

  (testing "Other values depend on (empty? value) to determine emptiness"
    (is (nil?                (identity->nil "")))
    (is (= "The quick brown" (identity->nil "The quick brown")))
    (is (nil?                (identity->nil [])))
    (is (= [:something]      (identity->nil [:something]))))

  (testing "The second parameter overrides the identity function"
    (is (nil?                (identity->nil 1         #{1})))
    (is (= 42                (identity->nil 42        #{1})))
    (is (nil?                (identity->nil "nil"     #{"nil" "none" " "})))
    (is (nil?                (identity->nil " "       #{"nil" "none" " "})))
    (is (nil?                (identity->nil "none"    #{"nil" "none" " "})))
    (is (= "Success"         (identity->nil "Success" #{"nil" "none" " "})))))


(deftest getter-test
  (testing "camelCase to getter"
    (is (= "getName"        (getter "name")))
    (is (= "getFirstName"   (getter "firstName"))))

  (testing "underscore_names to getter"
    (is (= "getFirstName"   (getter "first_name"))))

  (testing "hyphenated-names to getter"
    (is (= "getFirstName"   (getter "first-name")))
    (is (= "getThisIsATest" (getter "this-is-a-test")))))


(deftest keywordize-test
  (testing "'Some Pig' -> :some-pig"
    (is (= :some-pig             (keywordize "Some Pig"))))

  (testing "ThisIsATest -> :this-is-a-test and similar"
    (is (= :this-is-a-test       (keywordize "ThisIsATest")))
    (is (= :this-is-a-test       (keywordize "thisIsATest"))))

  (testing "' ', _, and / treated as delimiters and replaced with -"
    (is (= :this-is-a-test       (keywordize "This_Is/A/ test")))
    (is (= :thi$-is-a-test-123   (keywordize "thi$_Is/A/ test 123"))))

  (testing "nested.propertyName -> :nested.property-name"
    (is (= :nested.property-name (keywordize "nested.propertyName")))))


#_(run-tests)
