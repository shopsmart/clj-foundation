(defproject com.github.shopsmart/clj-foundation "0.9.14"
  :description "Common patterns enabling simpler to be easier and harder to be possibler."
  :url "https://github.com/shopsmart/clj-foundation"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-test-bang-bang "0.2.0"] ; lein test!! - Run each test NS in a separate JRE
            [lein-ancient "0.6.10"]       ; lein ancient - Check for outdated dependencies
            [lein-auto "0.1.2"]           ; e.g.: lein auto kbit   or lein auto test
            [lein-kibit "0.1.2"]          ; lein kibit - Linter that suggests more idiomatic forms
            ]

  ;; Used in clj-foundation.config unit tests
  :jvm-opts ["-DCONFIG-PROD=/tmp/_test-config-prod.edn"]

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.xml "0.0.8"]
                 [prismatic/schema "1.1.1"]
                 [potemkin "0.4.3"]])
