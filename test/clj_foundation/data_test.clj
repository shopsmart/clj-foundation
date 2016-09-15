(ns clj-foundation.data-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.unit-test-common :as common]
            [clj-foundation.data :refer :all]))


(common/register-fixtures)

;; FIXME!!! Move tests over from errors and patterns and test the rest!

(run-tests)
