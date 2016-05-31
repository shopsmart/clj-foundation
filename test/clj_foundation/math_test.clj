(ns clj-foundation.math-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.math :refer :all]))


(deftest mixednumber-parts-test
  (testing "Decimals can be turned into MixedNumbers and then into strings."
    (is (= "3 1/4" (str (->MixedNumber 3.25)))))

  (testing "Rationals can be turned into MixedNumbers and then into strings."
    (is (= "3 1/4" (str (->MixedNumber 13/4)))))

  (testing "When decomposed, MixedNumbers match the MixedNumberParts schema and have the correct values."
    (let [whole+frac (.decompose (->MixedNumber 13/4))
          frac-only  (.decompose (->MixedNumber 0.25))]

      (is (s/validate MixedNumberParts whole+frac))
      (is (= 3 (:whole whole+frac)))
      (is (= 1/4 (:frac whole+frac)))

      (is (s/validate MixedNumberParts frac-only))
      (is (= 0 (:whole frac-only)))
      (is (= 1/4 (:frac frac-only))))))

(run-tests)
