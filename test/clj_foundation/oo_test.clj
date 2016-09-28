(ns clj-foundation.oo-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.oo :refer :all]))


;; @see also http://stackoverflow.com/questions/9753164/generate-java-beans-with-clojure

(common/register-fixtures)

;; Test data

(definterface IAddress
  (getStreet [])
  (getStreet2 [])
  (getCity [])
  (getState [])
  (getPostalCode []))

(deftype Address [street street2 city state postal-code]
  IAddress
  (getStreet     [_] street)
  (getStreet2    [_] street2)
  (getCity       [_] city)
  (getState      [_] state)
  (getPostalCode [_] postal-code))

(definterface IPerson
  (getFirstName [])
  (getLastName [])
  (getAddress []))

(deftype Person [first-name last-name address]
  IPerson
  (getFirstName [_] first-name)
  (getLastName  [_] last-name)
  (getAddress   [_] address))

(def jd-address (Address. "42 Computer Blvd." "" "Acme" "AZ" "99999"))
(def john-doe (Person. "John" "Doe" jd-address))


;; Tests

(deftest bean-props-test
  (testing "Convert (possibly-nested) objects to a Map"
    ;; Preferred syntax uses keywords to name JavaBean properties
    (is (= {:first-name          "John"
            :last-name           "Doe"
            :address             jd-address}
           (bean-props john-doe :firstName :lastName :address)))

    (is (= {:first-name          "John"
            :last-name           "Doe"
            :address.street      "42 Computer Blvd."
            :address.street2     ""
            :address.city        "Acme"
            :address.state       "AZ"
            :address.postal-code "99999"}
           (bean-props john-doe :firstName :lastName :address.street :address.street2 :address.city :address.state :address.postalCode)))

    ;; Also works
    (is (= {:first-name          "John"
            :last-name           "Doe"
            :address.street      "42 Computer Blvd."
            :address.street2     ""
            :address.city        "Acme"
            :address.state       "AZ"
            :address.postal-code "99999"}
           (bean-props john-doe firstName lastName address.street address.street2 address.city address.state address.postalCode)))))



(run-tests)
