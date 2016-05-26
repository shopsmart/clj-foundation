# clj-foundation

Describes and specifies best practices at Brad's Deals for working in Clojure.

This library only depends on Clojure, Potemkin, and Schema.  Intentionally, it does not add any additional dependencies.

Libraries making additional simple things easy and hard things possible in Clojure that are intended for use across all Clojure projects at Brad's Deals.

For example:

* Variable and data structure declaration; common design pattern implementations
* Error processing and timeouts
* Easier serialization / deserialization / EDN resource reading
* Math extensions supporting mixed numbers and converting various units of time

This is where ShopSmart/Brad's Deals places Clojure language-level utilities that are
useful across all Clojure projects.

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


## License

Copyright © 2015 by ShopSmart, LLC.  Licensed under the Eclipse Public License v1.0.

## Authors

David Orme
