(ns clj-foundation.io
  (:require [schema.core :as s :refer [=> =>*]]
            [clj-foundation.errors :as err]
            [clj-foundation.templates :as template]
            [clj-foundation.patterns :as p]
            [clj-foundation.data :as data]
            [clojure.java.io :as io]
            [clojure.edn :as edn])

  (:import [java.io InputStream File ByteArrayInputStream ByteArrayOutputStream]
           [java.net URI URL Socket]))


;; From the deprecated clojure.contrib.io library
;; https://github.com/clojure/clojure-contrib/blob/b6c6f0fa165975c416c7d135d1a844353527493b/src/main/clojure/clojure/contrib/io.clj#L352
(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evaluates body.
  Anything printed within body will be written to f."
  [f & body]
  `(with-open [stream# (clojure.java.io/writer ~f)]
     (binding [*out* stream#]
       ~@body)))


(defmacro with-out-string
  "Evaluate body, capturing *out* to a string.  Returns contents of string."
  [f & body]
  `(let [baos# (ByteArrayOutputStream.)]
     (with-open [stream# (clojure.java.io/writer baos#)]
       (binding [*out* stream#]
         ~@body))
     (str baos#)))


(s/defn string-input-stream :- InputStream
  "Return a java.io.InputStream containing the input string"
  [input :- s/Str]
  (ByteArrayInputStream. (.getBytes input)))


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


;; Data types / schemas specifying the kinds of types from which we can stream data

(def ByteArray (Class/forName "[B"))
(def clojure-file-inputs
  "The data types allowed as input to io/input-stream"
  [s/Str InputStream File URI URL Socket ByteArray])


(def ResourcePath
  "A path to a JAR file resource on the classapth."
  s/Str)

(def EnvironmentVariable
  "The name of a java system variable or O/S environment variable (in that order) that may override
  a ResourcePath."
  s/Str)

(s/defschema ResourceOverrideVector
  "Read the resource at ResourcePath unless a Java system variable or an O/S EnvironmentVariable
   is present with the specified name.  In that case, use the value referenced by EnvironmentVariable
   as a file system path to the actual file."
  [(s/one EnvironmentVariable "Java system or O/S environment variable that may override the recource")
   (s/one ResourcePath "JAR resource on the classpath")])

(s/defschema ResourceOverrideMap
  "Read the file specified by :file or the resource specified by :resource.  If both entries are defined,
  the :file entry takes precedence over the :resource entry."
  {(s/optional-key :file) s/Str
   (s/optional-key :resource) s/Str})

(s/defschema ClojureFileInputs
  "The set of types Clojure allows for opening files/streams"
  (apply s/cond-pre clojure-file-inputs))

(s/defschema ExtendedFileInputs
  "Clojure's file inputs plus extensions for reading from either Jar resources or Files."
  (apply s/cond-pre ResourceOverrideVector ResourceOverrideMap clojure-file-inputs))


(defn- resolve-envar-override
  "If environment-variable is defined as a Java system variable or O/S variable (in that order), retrieve
  its value, and return that value, coerced to java.io.File.  Otherwise, return the original resource-path
  as a URL."
  [environment-variable resource-path]
  (let [config-envar-name (str "${" environment-variable "}")
        substitution-map {(keyword environment-variable) resource-path}
        file-location (template/subst<- config-envar-name substitution-map)]

    (if (= resource-path file-location)
      (io/resource file-location)
      (io/as-file file-location))))


(defn- resolve-override-map
  [{:keys [file resource]}]
  (cond
    file     (io/file file)
    resource (io/resource resource)
    :else    (throw (IllegalArgumentException. (str "Don't know how to open a file from: " map)))))


(defn- matches [schema value]
  (not (s/check schema value)))


;; File reading...

(s/defn normalize-file-input :- ClojureFileInputs
  "If the file-input is a string and can be converted to a resource URL, return that.
  If the file-input is a FileLocation, translate the :resource string to a URL and return that,
  otherwise return a File object wrapping the :file entry.  Otherwise, return the original file-input."
  [file-input :- ExtendedFileInputs]

  (let [result
        (cond
          (matches ResourceOverrideVector file-input) (apply resolve-envar-override file-input)
          (matches ResourceOverrideMap file-input)    (resolve-override-map file-input)
          :else file-input)]

    (try
      (s/validate ClojureFileInputs result)
      (if (string? result)
        (data/replace-nil (io/resource result) result)
        result)
      (catch Throwable e
        (throw (IllegalArgumentException.
                (str "Expected result to be one of " (s/explain ClojureFileInputs)
                     " but found " result)
                e))))))



(s/defn input-stream :- InputStream
  "Read and return a text file as an InputStream.  Supported sources are the same as io/input-stream, with
  the following additions: ResourceOverrideMap and ResourceOverrideVector.  See: (doc schema-name)
  for more information on these schema types."
  [file-input :- ExtendedFileInputs]
  (io/input-stream (normalize-file-input file-input)))


(s/defn read-file :- s/Str
  "Read and return a text file as a String.  Supported sources are the same as io/input-stream, with
  the following additions: ResourceOverrideMap and ResourceOverrideVector.  See: (doc schema-name)
  for more information on these types."
  [file-input :- ExtendedFileInputs]
  (with-open [input (input-stream file-input)]
    (slurp input)))


(s/defn resource-as-string :- s/Str
  "1-arg variant: Reads the specified classpath resource and returns its contents as a string.
   2-arg variant: [\"ENVAR\" \"resource-file.txt\"] - Allows resource-file to be overridden as-in read-file."
  [& resource-spec :- [s/Str]]
  (let [argc (count resource-spec)]
    (cond
      (= argc 1) (read-file {:resource (first resource-spec)})
      (= argc 2) (read-file [(first resource-spec) (second resource-spec)])
      :else (throw (IllegalArgumentException.
                    (str "resource-as-string: Illegal arg list: " resource-spec))))))


(defn- parse-extended-file-location [input subs]
  (if (odd? (count subs))
    [[input (first subs)] (rest subs)]
    [input subs]))


(s/defn read-template :- s/Str
  "Read a file from file-location, applying any variable substitutions specified
  in the file using the keyword/value pairs in substitutions.

  Supported sources are the same as io/input-stream, with the following additions:
  ResourceOverrideMap and ResourceOverrideVector.  See: (doc schema-name) and (explain schema-name)
  for more information on these types."

  [file-location :- ExtendedFileInputs & ex-substitutions]

  (let [[extended-file-location substitutions] (parse-extended-file-location file-location ex-substitutions)
        symbol-table (template/subst-map<- substitutions)
        file-contents-with-vars (read-file extended-file-location)
        resolved-file-contents (template/subst<- file-contents-with-vars symbol-table)]
    resolved-file-contents))
