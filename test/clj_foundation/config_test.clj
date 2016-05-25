(ns clj-foundation.config-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.config :refer :all]))

(s/set-compile-fn-validation! true)


(defconfig conf-prod-bad-nvar "PROOOOOD" "config.edn"
  :ENGLISH-GREETING "heyo")

(defconfig config-dev "POROOOD" "config.edn"
  :ENGLISH-GREETING "G'day mate!")

(defconfig config-prod "CONFIG-PROD" "config.edn"
  :ENGLISH-GREETING "G'day mate!")


(deftest read-config-element-test
  (io/copy (-> "config-prod.edn" io/resource slurp) (io/file "/tmp/config-prod.edn"))

  (testing "Read a setting from a file"
    (is (= "heyo" (conf-prod-bad-nvar :hello :english))))

  (testing "When environment variable is empty, default configuration file is used"
    (is (= "bonjour" (config-dev :hello :french))))

  (testing "When substitution variables are defined in the EDN file, they are substitued."
    (is (= "G'day mate!" (config-dev :hello :english))))

  (testing "When the default configuration file is overridden via an environment variable, its values are returned"
    (is (= "yo!" (config-prod :hello :english)))
    (is (= "salut" (config-prod :hello :french)))))


(run-tests)
