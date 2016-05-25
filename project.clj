(defproject com.github.shopsmart/clj-foundation "0.9.2"
  :description "Common patterns enabling simpler to be easier and harder to be possibler."
  :url "https://github.com/shopsmart/clj-foundation"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-test-bang-bang "0.2.0"]]

  :aot :all

  ;; Used in clj-foundation.config unit tests
  :jvm-opts ["-DCONFIG-PROD=/tmp/config-prod.edn"]

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [prismatic/schema "1.1.1"]
                 [potemkin "0.4.3"]])
