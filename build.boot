(def project 'bradsdeals/clj-foundation)
(def version "0.9.24")
(def project-name "clj-foundation")
(def project-openness :open-source)

(def description "Guiding opinions: Enhance the core language in resonable, useful, and conservative ways.
Don't be a framework.  Rather, be a conservative set of generally-useful functions that may be used
together or separately.  Make advanced topics like transducers and monads so easy that you don't have
to know when you're using them.  Use a small set of common-sense dependencies to minimize adoption friction.")
(def scm-url "https://github.com/shopsmart/clj-foundation")


(set-env! :resource-paths #{"resources"}
          :source-paths   #{"src" "test"}
          :repositories #(conj % ["clojars-push" {:url "https://clojars.org/repo/"
                                                  :username (System/getenv "CLOJARS_USER")
                                                  :password (System/getenv "CLOJARS_PASS")}])

          :dependencies   '[[org.clojure/clojure       "1.8.0"]
                            [clojure-future-spec       "1.9.0-alpha14"]

                            [io.aviso/pretty "0.1.30"]
                            [org.clojure/data.csv "0.1.3"]
                            [org.clojure/data.xml "0.0.8"]
                            [prismatic/schema "1.1.1"]
                            [potemkin "0.4.3"]

                            [bradsdeals/clj-boot    "LATEST" :scope "test"]])

(require '[clj-boot.core :refer :all])

(set-task-options! project project-name project-openness description version scm-url)
