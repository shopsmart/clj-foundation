# Introduction to clj-foundation

Clojure has a great standard library.  Even so, some things could be easier than they are.

## Why clj-foundation?

clj-foundation supplies namespaces making additional simple things easy and hard things possible in Clojure for functions needed across all Clojure projects at Brad's Deals.

The following describe our core goals/values for clj-foundation:

* Enhances the core language in resonable, useful, and conservative ways.
* Not a framework.  Rather, a conservative set of generally-useful functions that may be used together or separately.
* Makes it easy to create new transducers (or pipelines of transducers) from any regular function.
* Where applicable, enables programming using a monadic style without requiring explicit monad types.
   * Clojure already polymorphically supports mapcat over most core data types.  The missing thing is an implementation of the monadic zero as a separate and distinct thing from nil.
* Describes, specifies, and illustrates best practices at Brad's Deals for working in Clojure.
* The only dependencies are Clojure.*, Potemkin, io.aviso/pretty (for better stack traces), and Schema in order to minimize adoption friction.  In the near future we will migrate from Schema to Specs.


## Other Clojure foundational libraries that compliment this one

* Immutant: http://immutant.org/
* Hara: http://docs.caudate.me/hara/index.html


## High-quality domain-specific libraries complimenting this

* Cassandra as a mutable, versioned map: https://github.com/MyPost/cassius
* Git from the Clojure REPL: https://github.com/zcaudate/gita


## Curated directories of Clojure libraries

* http://clojurewerkz.org
* http://blog.takipi.com/the-top-100-clojure-libraries-in-2016-after-analyzing-30000-dependencies/
* https://github.com/razum2um/awesome-clojure
* http://www.clojure-toolbox.com/


# Features

The folowing is a small sample of clj-foundation's features:

## Math

* A Mixed Number type
* Time conversions to/from milliseconds
* (str (millis/->dmhs elapsed-time)) => e.g.: "5 days 2 hours 3 minutes 2 seconds"
* A Nothing type that behaves like the identity value for Maps, Seqs, and Strings and is distinct from nil.
* Functions converting identity values of various types/operations to and from nil.


## Ever had to declare a map containing the result of let variable bindings?  Now you can do that in one step with let-map.

```clojure
(let-map [meaning 42
          secrets (io/read-file "/etc/passwd")])
==>
{:meaning 42
 :secrets "nobody:*:-2:-2:Unprivileged User:/var/empty:/usr/bin/false\nroot:*:0:0:System Administrator:/var/root:/bin/sh\n...."}
```

## Data/error processing enhancements and timeout handling.

```clojure
;; Replace a nil value with a default
(replace-nil (:first-name customer) "John")

;; Returns value if non-nil else throws IllegalArgumentException naming the nil value
(not-nil (:first-name customer) "First name")

;; Expect a condition specified by a predicate to become true within timeout-millis.
;; Throws IllegalStateException on failure with the specified message.
(expect-within
  (millis/<-seconds 5)
  (fn [] (= (meaning-of-life) 42))
  "Couldn't compute the meaning of life.")

;; Retry x times, with millis puase interval a specified function that is failure-prone
(retry 5 (millis/<-seconds 10) some-unreliable-io-operation)
```

## A string interpolation language that is intentionally dumb about the syntax of its hosting language.

```clojure
;; Given:
(def content "Say hello to ${NAME}")

;; Then:
(templates/subst<- content :NAME "Jack")
==>
"Say hello to Jack"

(subst<- content :ZIP "46989")
==>
ExceptionInfo Not found: '${NAME}'  clojure.core/ex-info

(interpolation-vars content)
==>
(:NAME)
```

* Interpolation variables are resolved from the following places with the following precedence:
    * Java system variables (e.g.: java -DNAME='Jack' com.foo.bar.YourMainProgram)
    * Operating system environment variables
    * Any key/value variables specified in code.

## Extensions to Clojure's file input functions

```clojure
;; Return a Jar resource as a string
(resource-as-string "config.txt")

;; Return a Jar resource as a string, but allow the ALT_CONFIG_LOCATION Java system or O/S environment
;; variable to redirect reading to an external configuration file instead.
(resource-as-string "ALT_CONFIG_LOCATION" "config.txt")

;; Return a Jar resource as a string, interpolating any template variables inside the Jar resource
;; using the supplied interpolation variables and the override rules defined by the template language.
(read-template "config.properties" :NAME "Jack")

;; Return a Jar resource as a string, possibly overridden by the environment variable ALT_CONFIG_LOCATION
;; per the template engine's precedence rules.  Any template variables in the file will also be substuted
;; using the supplied key/value pairs and/or matching environment variables per the template engine's
;; precedence rules.
(read-template "ALT_CONFIG_LOCATION" "config.properties" :NAME "Jack")
```

## And more...

* Easier serialization / deserialization in various formats.  Currently supports binary and EDN formats.  Optionally can use the template language to support substitution variables inside EDN files.
* A let-map form similar to "let" that returns its names and values as a Map--for those times the values of subsequent keys in a Map depend on the values of prior ones.
* Simple named implementations of design patterns familiar to Java developers, particularaly Singleton.

# Deployment/usage

## Leiningen coordinates

```clojure
:user {:repositories [["jitpack" "https://jitpack.io"]]

       :dependencies [[com.github.shopsmart/clj-foundation "version"]]}
```

where "version" currently is "[![Release](http://jitpack.io/v/com.github.shopsmart/clj-foundation.svg)](https://jitpack.io/#shopsmart/clj-foundation)".

## Maven coordinates

```xml
<repositories>
  <repository>
    <id>jitpack.io</id>
    <name>Jitpack repo</name>
    <url>https://jitpack.io</url>
  </repository>
</repositories>
```

* GroupId: com.github.shopsmart
* ArtifactId: clj-foundation
* Version: [![Release](http://jitpack.io/v/com.github.shopsmart/clj-foundation.svg)](https://jitpack.io/#shopsmart/clj-foundation)

## Manual build/test

```bash
# Build
$ lein jar

# Test
$ lein with-profile test test
```
