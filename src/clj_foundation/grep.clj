(ns clj-foundation.grep
  "Unix-style grep for arbitrary Clojure nested collections."
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.string :as str]
            [clj-foundation.tree-visit :as v]
            [clj-foundation.errors :refer [must-be]]
            [clj-foundation.patterns :refer [nothing arity]]))


(defn- update-breadcrumb [old-path loc]
  (let [parent (v/parent-container loc)
        new-depth (v/depth loc)
        new-index (v/index loc)
        new-node (zip/node loc)]
    (if (or (map? new-node) (v/root-node? (zip/up loc)))
      old-path
      (if (map? parent)
        (if (not (v/traversable-coll? new-node))
          (conj (vec (take (- new-depth 1) old-path)) new-node)
          old-path)
        (conj (vec (take (- new-depth 1) old-path)) new-index)))))


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
               (instance? java.util.regex.Pattern pattern) (fn [node] (re-find pattern (.toString node)))
               (string? pattern)                           (fn [node] (.contains (.toString node) pattern))
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
      (swap! breadcrumb update-breadcrumb loc)
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
