(ns clj-foundation.io
  (:require [schema.core :as s :refer [=> =>*]]
            [clojure.java.io :as io]))


;; From the deprecated clojure.contrib.io library
;; https://github.com/clojure/clojure-contrib/blob/b6c6f0fa165975c416c7d135d1a844353527493b/src/main/clojure/clojure/contrib/io.clj#L352
(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evalutes body.
  Anything printed within body will be written to f."
  [f & body]
  `(with-open [stream# (clojure.java.io/writer ~f)]
     (binding [*out* stream#]
       ~@body)))


(defn serialize
  "Print a data structure to a file so that we may read it in later."
  [#^String filename data-structure]
  (with-out-writer
    (java.io.File. filename)
    (binding [*print-dup* true] (prn data-structure))))


(defn deserialize [filename]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. filename))]
    (read r)))


;; Note: Records and such must define a serialVersionUID field for this to support migrations
(defn bin-serialize [filename o]
    (with-open [outp (-> (java.io.File. filename) java.io.FileOutputStream. java.io.ObjectOutputStream.)]
        (.writeObject outp o)))


(defn bin-deserialize [filename]
    (with-open [inp (-> (java.io.File. filename) java.io.FileInputStream. java.io.ObjectInputStream.)]
        (.readObject inp)))


(s/defn read-resource :- s/Str
  "Read a resource file, with an optional override to a file in the filesystem"
  [resource :- s/Str & override-file :- [(s/optional s/Str "Optional override file")]]
  (if-let [optional-override-location (first override-file)]
    (-> optional-override-location io/input-stream slurp)
    (-> resource io/resource slurp)))
