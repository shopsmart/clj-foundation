# clj-foundation

## Why clj-foundation?

clj-foundation supplies namespaces making additional simple things easy and hard things possible in Clojure that are intended for use across all Clojure projects at Brad's Deals.

* Enhances the core language in resonable, useful, and conservative ways.
* Enables programming using a monadic style without requiring explicit monad types.
* Describes, specifies, and illustrates best practices at Brad's Deals for working in Clojure.
* The only dependencies are Clojure, Potemkin, and Schema in order to minimize adoption friction.

## Other Clojure foundational libraries that compliment this one

* Hara: http://docs.caudate.me/hara/index.html


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


# Definition of Done for 1.0 release

* On APIs - While 1.0 will be a major release with some inevitable API breakage, we will strive:
    * To maintain API compatiblity with prior releases to the greatest extent possible
    * To remove duplicated functionality (which may contradict the API compatibility principle)
    * To maintain API compatibility with code copied from internal Brad's Deals projects to the greatest extent possible.
    * Clear naming convention for namespaces that are provisional (non-frozen, WIP) API.

* Known tech debt that may influence APIs
    * Consider migrating db.clj's local configuration mechanism to config.clj (and then make an instance of the generic implementation inside db.clj)

* Target Clojure 1.9
    * All functions will have type information supplied via Clojure 1.9 specs.
    * Functions currently using plumatic/schema to specify type constraints will be migrated to Specs

* On testing
    * All functions will be unit tested using a style designed to illustrate behavior under failure modes as well as happy path scenarios.
    * We will collectively agree on a style of testing that seems to hit the sweet spot between overspecification and underspecification.
        * (Details are not set in stone but definitely up for negotiation)
    * When defects are detected, we will first reproduce the defect's root cause using a failing test case that will prevent the defect from reoccurring without the test notifying us.
    * Generative testing with Specs
    * Integrate kbitz (suggest Clojure idioms), some Clojure linting library; if it's easy to make these available to clients of clj-foundation, do so.

* Debug library
    * dbg, ppdbg macros
    * trace?

* Documentation
    * Machine generate as much as possible?

* Make clj_infrastructure as a separate library
    * Move db.clj there?
    * Abstract db.clj API using monads at the foundation layer and implement for relational, nosql, etc. in infrastructure?
        * http://rea.tech/the-worst-thing-in-our-scala-code-futures/  ?
        * Abstractions for creating/consuming lazy sequences?
        * Abstractions for aggregating a sequence of database results into a map[concatinated-key value(s)]

## Provisional ideas for 1.0

* Figure out how to isolate namespaces and their dependencies at the classpath level, including runtime reloading and evolution semantics after the fashion of OSGi at the REPL or in a running application.
* Update and include clojure.osgi for clean deployment into OSGi frameworks?
* Deploy to Clojars?




## License

Copyright © 2015, 2016 by ShopSmart, LLC.  Licensed under the Eclipse Public License v1.0.


# Authors

* David Orme
* Levi Dixon
