(ns clj-foundation.grep
  "Unix-style grep for arbitrary Clojure nested collections.  Returns container(s) with at
  least one element matching a literal, regex, substring, or predicate.

  Examples:

  (grep 42         {:1 {:a 11} :2 {:b 42}})                ==> {:b 42}
  (grep #\"[0-9]\" [{:a \"42\"} {:b \"Hello, world\"}])    ==> {:a \"42\"}
  (grep \"world\"  [{:1 \"Hello\"} {:1 \"Hello, world\"}]) ==> {:1 \"Hello, world\"}
  (grep zero?      [[1 2 3] [4 5 6] [7 8 9] [0 1 2]])      ==> [0 1 2]"
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.string :as str]
            [clj-foundation.tree-visit :as v]
            [clj-foundation.errors :refer [must-be]]
            [clj-foundation.patterns :refer [nothing arity]]))


(defn- found-match-container
  "Add a result to state containing the breadcrumb and the parent container of the matched node."
  [state breadcrumb loc]
  {:state (conj state [breadcrumb (v/parent-container loc)])})


(defn- found-match-node
  "Add a result to state containing the breadcrumb and the clojure.zip visitor node referencing
  the match."
  [state breadcrumb loc]
  {:state (conj state [breadcrumb loc])})


(defn- grep-predicate
  [pattern & node-transformers]
  (let [pred (cond
               (instance? java.util.regex.Pattern pattern) (fn [node] (re-find pattern (str node)))
               (string? pattern)                           (fn [node] (.contains (str node) pattern))
               (fn? pattern)                               (do (must-be "Grep pattern functions must be arity 1" (= (arity pattern) 1))
                                                               pattern)
               :else                                       (fn [node] (= pattern node)))]
    (apply comp pred (reverse node-transformers))))


(defn- grep-match?-fn
  "Returns a function that determines if the current node is a match using the following
  algorithm:

  * Node-transformers (if specified) are first applied to the node in the order specified.
  * The pattern predicate is then applied to the transformed result."
  [pattern & node-transformers]
  (let [match? (apply grep-predicate pattern node-transformers)]
    (fn [node state loc breadcrumb found-match-fn]
      (when-not (or (v/traversable-coll? node)
                    (nil? node))
        (when (match? node)
          (found-match-fn state breadcrumb loc))))))


(defn- grep-tree-visitor
  "Create a visitor callback function--called for every node in the source node's graph.
  This function maintains the state of the breadcrumb vector and accumulates matches
  in the state vector."
  [pattern found-match-fn & node-transformers]
  (let [grep-match? (apply grep-match?-fn pattern node-transformers)
        breadcrumb (atom [])]
    (fn [node state loc]
      (swap! breadcrumb v/update-breadcrumb loc)
      (grep-match? node state loc @breadcrumb found-match-fn))))


(defn grep
  "Recursively grep the contents of node for elements matching pattern.  Pattern can be
  any type:

  * If pattern is a regular expression, it is matched against (.toString obj).
  * String patterns match any substring of (.toString obj).
  * If pattern is a function then the truthiness of (pattern node) determines matches.
  * All other types must match literally.

  node-transformers (if specified) are arity 1 functions (e.g.: clojure.string/upper-case) that
  transform the node before the pattern is matched against it.  These are applied in the order
  specified before matching.

  Returns [[breadcrumb1 match1-parent] [breadcrumb2 match2-parent] ... [breadcrumbn matchn-parent]]"
  [pattern node & node-transformers]
  (:state (v/tree-visitor
           (v/tree-zipper node)
           []
           [(apply grep-tree-visitor pattern found-match-container node-transformers)])))


(defn grep-nodes
  "Recursively grep the contents of node for elements matching pattern.  Pattern can be
  any type:

  * If pattern is a regular expression, it is matched against (.toString obj).
  * String patterns match any substring of (.toString obj).
  * If pattern is a function then the truthiness of (pattern node) determines matches.
  * All other types must match literally.

  node-transformers (if specified) are arity 1 functions (e.g.: clojure.string/upper-case) that
  transform the node before the pattern is matched against it.  These are applied in the order
  specified before matching.

  Returns [[breadcrumb1 match1-node] [breadcrumb2 match2-node] ... [breadcrumbn matchn-node]]

  where the node objects are the clojure.zip visitor nodes corresponding to the matches.

  Use the functions inside clojure.zip to traverse the object graph from the match node locations
  and to retrieve the value(s) you want to manipulate."
  [pattern node & node-transformers]
  (:state (v/tree-visitor
           (v/tree-zipper node)
           []
           [(apply grep-tree-visitor pattern found-match-node node-transformers)])))
