(ns clj-foundation.data
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clj-foundation.patterns :as p :refer [types f]]
            [clj-foundation.errors :as err]
            [schema.core :as s :refer [=> =>* defschema]]
            [potemkin :refer [def-map-type]])
  (:import [java.io StringReader]
           [clojure.lang Named])
  (:gen-class))


;; Predicates and data conversion functions -------------------------------------------------

(defn any?
  "Returns coll if any element in coll satisfies predicate."
  [predicate coll]
  (when (not-empty (filter predicate coll))
    coll))


(s/defn replace-if :- s/Any
  "In the binary form, replace value with the result of running predicate against value
   if that result is truthy.  In the terniary form, replace value with replacement if
   the result of running predicate against value is truthy.

   An idiomatic way to use this function is to pass a set of disallowed values as
   predicate, and a replacement value as replacement."

  ([value :- s/Any, predicate :- (=> s/Any [s/Any])]
   (let [maybe-result (predicate value)]
     (or
       maybe-result
       value)))

  ([value :- s/Any, predicate :- (=> s/Any [s/Any]), replacement :- s/Any]
   (if (predicate value)
     value
     replacement)))


(s/defn replace-nil :- s/Any
  "Accepts a value that cannot be nil; if it is not nil it returns it, else it
  returns its replacement."
  [maybe-nil :- s/Any, replacement :- s/Any]
  (replace-if maybe-nil #{nil} replacement))


(s/defn nothing->identity :- s/Any
  "Takes nil or Nothing to the specified identity value for the type and computation in context,
  otherwise returns value.  An identity value can be applied to a value of the given type under the
  operation in context without affecting the result.  For example 0 is the identity value for rational
  numbers under addition.  The empty string is the identity value for strings under concatination.

  Note that Nothing is already an identity value for maps and seqs.  This function is only useful
  for types where the Nothing type is ill-behaved (e.g.: Strings, Numbers, ...) for a given operation.

  Another name for this concept is the monadic zero for the type/operation."
  [identity-value :- s/Any, value :- s/Any]

  (if (p/something? value)
    value
    identity-value))


(s/defn identity->nil :- s/Any
  "Synopsis:
     (identity->nil [])                                  --> nil
     (identity->nil \"\")                                --> nil
     (identity->nil 1 #{1})                              --> nil   ; e.g.: Under multiplication
     (identity->nil \"none\" #{\"nil\" \"none\" \" \"})  --> nil

  If value is empty (for its type's natural definition of empty), returns nil.  Otherwise returns
  value.

  * Non-numeric values are empty iff (empty? value).
  * Numbers default to zero as their identity value.
  * The identity predicate may optionally be overridden in the second parameter."
  ([value      :- s/Any
    identity-p :- (=> s/Bool [s/Any])]
   (when-not (identity-p value) value))

  ([value      :- s/Any]
   (cond
     (number? value) (identity->nil value zero?)
     :else           (identity->nil value empty?))))


;; FIXME: The following two do roughly the same thing.  Consolidate.

(s/defn value-or :- s/Any
  "If value is nil or an instance of Nothing, run f and return its result.  Else, return value."
  [value :- s/Any, f :- (=> s/Any [s/Any])]
  (if (or (nil? value)
          (instance? (p/Nothing!) value))
    (f value)
    value))


(s/defn something-or :- s/Any
  "If value is not Nothing return value, else run f and return its result."
  [value :- s/Any, f :- (=> s/Any [s/Any])]
  (if (p/something? value)
    value
    (f value)))



(s/defn undasherize :- s/Str
  "Replace all instances of '-' or '_' with replacement"
  [replacement :- s/Str,  value :- (types s/Str Named)]
  (str/replace (name value) #"[\-_]" replacement))


(s/defn getter :- s/Str
  [property-name :- s/Str]
  "Translate property-name to its Java getter syntax.  Handles hyphenated-names and underscore_names as well as
  names that are already camelCase."
  (->> (str/split property-name #"[\_-]")
       (map (f part =>
               (str (str/upper-case (first part))
                    (apply str (rest part)))))
       (str/join)
       (str "get")))


(s/defn ->SNAKE_CASE :- s/Str
  "Convert any Named or String object to SNAKE_CASE.  Does not handle camelCase."
  [value :- (types s/Str Named)]
  (str/upper-case (undasherize "_" (name value))))


(s/defn ->uppercase-with-spaces :- s/Str
  "Convert - or _ to ' ' and captialize string."
  [value :- (types s/Str Named)]
  (str/upper-case (undasherize " " (name value))))


(defn dasherize
  "Replace all instance of match with '-' in value."
  [match value]
  (str/replace (str value) match "-"))


(defn keywordize
  "Return dasherized keyword from camelCase underscore_names, namespaced/names, etc.
  See the unit tests for the complete set of supported cases.

  Ex. camelCase          -> :camel-case
      some_name          -> :some-name
      customer.firstName -> :customer.first-name"
  [name]
  (->> name
       (re-seq #"([A-Z _/]?[a-z1-9$\.]*)") ; Seq of tokens: Leading delimeter + following chars
       (map first)                         ; Take 1st element of each tuple in match seq
       (map #(str/replace % #"[ _/]" ""))  ; Eliminate explicit delimeter characters
       (filter #(not (empty? %)))          ; Some tokens will now be empty; throw them out
       (str/join "-")                      ; Back to a string, joined by "-"
       (str/lower-case)                    ; ...
       (keyword)))


(s/defn string->keyword :- s/Keyword
  "Convert string name to a keyword respecting naming-exceptions.
   Ex. some_name -> :some-name"
  ([name :- s/Str
    naming-exceptions :- #{s/Str}]
    (if-let [name-exception (naming-exceptions name)]
      name-exception
      (keywordize name)))
  ([name :- s/Str]
    (keywordize name)))


(defn string-list->keywords
  "Convert a list of strings to a list of keywords"
  ([list]
    (map (fn [string]
         (string->keyword (name string))) list))
  ([list naming-exceptions]
    (map (fn [string]
         (string->keyword (name string) naming-exceptions)) list)))


(defn constant-seq
  "Return an infinite lazy seq of cs"
  [c]
  (lazy-seq (cons c (constant-seq c))))


(s/defn set-map-entries :- {s/Any s/Any}
  "Returns a new copy of m where for all [k v] => if k is in entries, v is set to new-value."
  [m         :- {s/Keyword s/Any}
   entries   :- [s/Keyword]
   new-value :- s/Any]
  (let [new-entries (zipmap entries (constant-seq new-value))]
    (merge m new-entries)))


(defn remove-header-row
  "When processing CSV, frequently we want to throw out the header row."
  [data]
  (rest data))


(defn parse-xml
  "Parse XML from a String"
  [xml]
  (xml/parse (StringReader. xml)))


(defn xml-element->kv-pair
  "Return key-value pair where key is the tag name,
   and value is content of the xml element."
  [element]
  [(:tag element)
   (first (:content element))])


(defn xml-elements->map
  "Return a map of key-value pairs from the given xml elements."
  [elements]
  (into {} (map xml-element->kv-pair (:content elements))))


(defn extract-root-xml-element
  "Extract root xml element from the given XML document."
  [root-element xml]
  (let [initial-tag (:tag xml)]
    (if (= root-element initial-tag)
      (:content (first (:content xml)))
      (throw (java.lang.IllegalStateException. (str "Invalid initial XML tag: " initial-tag))))))


;; Simplify Java interop --------------------------------------------------------------------


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


#_(defmacro bean<-
  [class kv-map])
