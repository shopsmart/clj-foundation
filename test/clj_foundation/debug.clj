;;; debug_repl.clj -- Clojure REPL that is aware of surrounding
;;; lexical scope

;; Original version written by George Jahad, with brilliant
;; improvements by Alex Osborne

;; Jan 31, 2010

;;  Copied from https://github.com/GeorgeJahad/debug-repl
;;  which hasn't been maintained for 3 years.

;; Other good tools for debugging:
;; * https://github.com/jonase/kibit
;; * https://github.com/dgrnbrg/spyscope
;; * https://github.com/zcaudate/vinyasa

;; And debug tooling docs...
;;  * http://brownsofa.org/blog/2014/08/03/debugging-in-clojure-tools/
;;  * http://z.caudate.me/give-your-clojure-workflow-more-flow/
;;  * http://dev.solita.fi/2014/03/18/pimp-my-repl.html

;; Copyright (c) George Jahad, Alex Osborne and Contributors. All
;; rights reserved.  The use and distribution terms for this software
;; are covered by the Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.  You must not remove this notice, or any
;; other, from this software.

;; Contributors:
;; Konrad Hinsen

(ns clj-foundation.debug
  [:require clojure.main])

(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(declare ^:dynamic *locals*)

(defn view-locals []
  *locals*)

(defn eval-with-locals
  "Evals a form with given locals. The locals should be a map of symbols to
values."
  [locals form]
  (binding [*locals* locals]
    (eval
     `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
        ~form))))

(defn dr-read
  [request-prompt request-exit]
  (let [input (clojure.main/repl-read request-prompt request-exit)]
    (if (= input '())
      request-exit
      input)))

(def ^:dynamic level 0)
(def counter (atom 1000))
(defn inc-counter []
  (swap! counter inc))

(def element (atom nil))

(def quit-dr-exception
     (proxy [Exception java.util.Enumeration] []
       (nextElement [] @element)))

(defn quit-dr [ & form]
  (reset! element (first form))
  (throw quit-dr-exception))

(def ^:dynamic  exit-dr-exception
     (Throwable. "Exiting back to main repl from debug-repl"))

(defn exit-dr []
  (throw exit-dr-exception))

(defn caught [exc]
  (cond
    (= (.getCause exc) quit-dr-exception) (throw quit-dr-exception)
    (= (.getCause exc) exit-dr-exception) (throw exit-dr-exception)
    :else (clojure.main/repl-caught exc)))

(defmacro debug-repl
  "Starts a REPL with the local bindings available."
  ([]
     `(debug-repl nil))
  ([form]
     `(let [counter# (inc-counter)
            eval-fn# (partial eval-with-locals (local-bindings))]
        (try
         (binding [level (inc level)]
           (clojure.main/repl
            :prompt #(print (str "dr-" level "-" counter# " => "))
            :eval eval-fn#
            :read dr-read
            :caught caught))
         (catch Exception e#
           (cond
             (= e# quit-dr-exception)
             (if-let [new-form# (.nextElement quit-dr-exception)]
               (eval-fn# new-form#)
               (eval-fn# ~form))
             (= e# exit-dr-exception)
             (when (> level -1)
               (throw exit-dr-exception))
             :else (throw e#)))))))


(comment
  ;;
  ;; Some examples
  ;;

  (let [c 1
        locals (local-bindings)
        d 2]
    locals)
  ;; => {fn__5310 #<user$eval__5309 user$eval__5309@1a65a18>, c 1}

  (let [c 1
        d 2]
    (defn a [b c]
      (debug-repl)
      d))
  (a "foo" "bar")
  ;; dr => c
  ;; "bar"
  ;; dr => d
  ;; 2
  ;; dr => *locals*
  ;; {fn__20 #<user$eval__19 user$eval__19@955cd5>
  ;; c "bar"
  ;; d 2
  ;; fn__22 #<user$eval__19$a__21 user$eval__19$a__21@59fb21>
  ;; b "foo"}


  user=> (let [a 10] (debug-repl (* a a)))
  dr-1-1006 => (quit-dr)
  100

  user=> (let [a 10] (debug-repl (* a a)))
  dr-1-1007 => (quit-dr 99)
  99

  )
