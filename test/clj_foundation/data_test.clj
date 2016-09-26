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


;; FIXME!!! Move tests over from errors and patterns and test the rest!


(run-tests)
