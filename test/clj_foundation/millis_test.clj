(ns clj-foundation.millis-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.millis :refer :all]))


(common/register-fixtures)

(def unit-conversions
  "Conversions from a larger unit to millis, then back to the larger unit."
  [[<-seconds ->seconds]
   [<-minutes ->minutes]
   [<-hours ->hours]
   [<-days ->days]])


(deftest unit-conversions-reverse-correctly
  (testing "Every unit-conversion reverses itself to an appropriate mixed number."
    (is (every?
         (fn [[<-larger ->larger]]
           (= "1 1/4"
              (str (-> 1.25 <-larger ->larger))))
         unit-conversions))))


(deftest dhms-test
  (testing "dhms decomposes to an appropriate mixed number value for each larger unit"
    (let [millis (+ (<-days 5) (<-hours 3) (<-minutes 29) (<-seconds 33) 500)
          dhms (->dhms millis)
          parts (.decompose dhms)]
      (is (= 5 (:whole (.decompose (:days parts)))))
      (is (= 3 (:whole (.decompose (:hours parts)))))
      (is (= 29 (:whole (.decompose (:minutes parts)))))
      (is (= "33 1/2" (str (:seconds parts))))))

  (testing "A dhms string represents the appropriate precision"
    (is (= "5d 3h 29m 33s" (str (->dhms (+ (<-days 5) (<-hours 3) (<-minutes 29) (<-seconds 33) 500)))))
    (is (= "3h 29m 33s"    (str (->dhms (+ (<-hours 3) (<-minutes 29) (<-seconds 33) 500)))))
    (is (= "29m 33s"       (str (->dhms (+ (<-minutes 29) (<-seconds 33) 500)))))
    (is (= "33s"           (str (->dhms (+ (<-seconds 33) 500)))))))


(run-tests)
