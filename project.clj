(defproject com.github.shopsmart/clj-foundation "0.9.22"
  :description "Guiding opinions: Enhance the core language in resonable, useful, and conservative ways.
Don't be a framework.  Rather, be a conservative set of generally-useful functions that may be used
together or separately.  Make advanced topics like transducers and monads so easy that you don't have
to know when you're using them.  Use a small set of common-sense dependencies to minimize adoption friction.

The library is hosted on jitpack.io, so you will need: :repositories [[\"jitpack\" \"https://jitpack.io\"]]"

  :url "https://github.com/shopsmart/clj-foundation"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[lein-test-bang-bang "0.2.0"] ; lein test!! - Run each test NS in a separate JRE
            [lein-ancient "0.6.10"]       ; lein ancient - Check for outdated dependencies
            [lein-auto "0.1.2"]           ; e.g.: lein auto kbit   or lein auto test
            [lein-kibit "0.1.2"]          ; lein kibit - Linter that suggests more idiomatic forms
            [lein-codox "0.10.1"]         ; lein codox - Generate documentation
            ]

  ;; Used in clj-foundation.config unit tests
  :jvm-opts ["-DCONFIG-PROD=/tmp/_test-config-prod.edn"]

  :codox {:metadata {:doc/format :markdown}
          :output-path "docs"
          :exclude-vars nil
          :source-uri "https://github.com/shopsmart/clj-foundation/blob/{version}/{filepath}#L{line}"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [io.aviso/pretty "0.1.30"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/data.xml "0.0.8"]
                 [prismatic/schema "1.1.1"]
                 [potemkin "0.4.3"]])
