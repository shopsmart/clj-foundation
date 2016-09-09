# clj-foundation

Libraries making additional simple things easy and hard things possible in Clojure that are intended for use across all Clojure projects at Brad's Deals.

* Describes, specifies, and illustrates best practices at Brad's Deals for working in Clojure.
* Contains namespaces intended to extend the core language.
* The only dependencies are Clojure, Potemkin, and Schema in order to minimize adoption friction.

## Features

The folowing is a small sample of clj-foundation's features:

### Ever had to declare a map as the result of a bunch of let variable bindings?  Now you can do that in one step with let-map.

```clojure
(let-map [meaning 42
          secrets (io/read-file "/etc/passwd")])
==>
{:meaning 42
 :secrets "nobody:*:-2:-2:Unprivileged User:/var/empty:/usr/bin/false\nroot:*:0:0:System Administrator:/var/root:/bin/sh\n...."}
```

### Error processing and timeout handling.

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

### A string interpolation language that is intentionally dumb about the syntax of its hosting language.

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
** Java system variables (e.g.: java -DNAME='Jack' com.foo.bar.YourMainProgram)
** Operating system environment variables
** Any key/value variables specified in code.

### Extensions to Clojure's file input functions

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

### And more...

* Easier serialization / deserialization in various formats.  Currently supports binary and EDN formats.  Optionally can use the template language to support substitution variables inside EDN files.
* A let-map form similar to "let" that returns its names and values as a Map--for those times the values of subsequent keys in a Map depend on the values of prior ones.
* Math extensions supporting mixed numbers.
* Time conversions to/from milliseconds; convert milliseconds into a single days, hours, minutes, seconds value.
* Simple named implementations of design patterns familiar to Java developers, particularaly Singleton.
* An implementation of None from the Option pattern (named Nothing) that behaves like the identity value in many contexts.

### Leiningen coordinates

```clojure
:user {:repositories [["jitpack" "https://jitpack.io"]]

       :dependencies [[com.github.shopsmart/clj-foundation "version"]]}
```

where "version" currently is "[![Release](http://jitpack.io/v/com.github.shopsmart/clj-foundation.svg)](https://jitpack.io/#shopsmart/clj-foundation)".

### Maven coordinates

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

### Manual build/test

```bash
# Build
$ lein jar

# Test
$ lein with-profile test test
```

## License

Copyright © 2015, 2016 by ShopSmart, LLC.  Licensed under the Eclipse Public License v1.0.


## Authors

David Orme
