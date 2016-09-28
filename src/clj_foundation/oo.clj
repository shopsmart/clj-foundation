(ns clj-foundation.oo
  "Simplify Java interop"
  (:require [clojure.string :as str]
            [clj-foundation.data :refer [keywordize getter]]))


(defn- prop-name [prop] (if (keyword? prop) (name prop) (str prop)))

(defn- getter-seq [prop]
  (let [property-name (prop-name prop)]
    (map symbol (map getter (str/split property-name #"\.")))))


(defmacro bean-props
  "Like (bean... but allows specifying the properties to convert and allows chained nested properties.
  Property names are translated to idiomatic hyphenated Clojure keywords in the resulting Map.

  e.g.: Given the following beans:
  (def jd-address (Address. \"42 Computer Blvd.\" \"\" \"Acme\" \"AZ\" \"99999\"))
  (def john-doe (Person. \"John\" \"Doe\" jd-address))

  then:

  (bean-props john-doe :firstName :lastName :address)
  ==>
  {:first-name          \"John\"
   :last-name           \"Doe\"
   :address             <some address object>}

  One step farther:

  (bean-props john-doe :firstName :lastName :address.street :address.street2 :address.city :address.state :address.postalCode)
  ==>
  {:first-name          \"John\"
   :last-name           \"Doe\"
   :address.street      \"42 Computer Blvd.\"
   :address.street2     \"\"
   :address.city        \"Acme\"
   :address.state       \"AZ\"
   :address.postal-code \"99999\"}"
  [object & props]
  (->> (if (seq props) props [])
       (map (fn [prop] [(keywordize (prop-name prop)) `(.. ~object ~@(getter-seq prop) )]))
       (into {})))


#_(defmacro object
  "Create the Java object specified by clazz using the specified (optional) ctor-args vector as
  constructor arguments and the field-values map to initialize JavaBean style properties.

  Wherever literal values are supplied, they are type checked by the macro at compile time.

  e.g.:

  (object Customer [1] {:first-name \"John\" :last-name \"Doe\"})"

  ([clazz field-values])
  ([clazz ctor-args field-values]))


#_(defmacro set-fields!
  "Set the JavaBean style fields of the specified object to the kvs specified in the
  field-values map.

  The keys in field-values may be :camelCase (without \"set\" at the beginning) or
  Clojure-style :hyphenated-words."
  [object field-values])
