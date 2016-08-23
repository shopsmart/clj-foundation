(ns clj-foundation.grep
  "Unix-style grep for arbitrary Clojure nested collections."
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [clj-foundation.tree-visit :as v]
            [clj-foundation.patterns :refer [nothing]])
  (:import [java.util Map Map$Entry]))


(defn- result-coll?
  "Don't return Map$Entrys as results; other collection types are supported."
  [node]
  (and (not (instance? Map$Entry node))
       (or (map? node) (vector? node) (list? node) (set? node) (seq? node))))


(defn- traversable-coll?
  "Is this a kind of collection we can traverse?"
  [node]
  (or (map? node) (vector? node) (list? node) (set? node) (seq? node)))


(defn root-node?
  "Is this node at the root of the collection?  (nil? is redundant but explicit)"
  [n]
  (nil? (zip/up n)))


(defn- parent-container
  "Return's the current node's parent container.  If the current node is the
  root node, returns the current node."
  [loc]
  (let [parent-result (some-> loc zip/up zip/node)]
    (cond
      (root-node? loc)             (zip/node loc)
      (result-coll? parent-result) parent-result
      :else                        (parent-container (zip/up loc)))))


(defn- depth [loc]
  (if (root-node? loc)
    0
    (+ (if (instance? Map$Entry (zip/node loc))
         0
         1)
       (depth (zip/up loc)))))


(defn- index [loc]
  (count (zip/lefts loc)))


(defn- update-breadcrumb [old-path loc]
  (let [parent (parent-container loc)
        new-depth (depth loc)
        new-index (index loc)
        new-node (zip/node loc)]
    (if (or (map? new-node) (root-node? (zip/up loc)))
      old-path
      (if (map? parent)
        (if (not (traversable-coll? new-node))
          (conj (vec (take (- new-depth 1) old-path)) new-node)
          old-path)
        (conj (vec (take (- new-depth 1) old-path)) new-index)))))


(defn- found-match-container
  "Add a result to state containing the breadcrumb and the parent container of the matched node."
  [state breadcrumb loc]
  {:state (conj state [breadcrumb (parent-container loc)])})


(defn- found-match-node
  "Add a result to state containing the breadcrumb and the clojure.zip visitor node referencing
  the match."
  [state breadcrumb loc]
  {:state (conj state [breadcrumb loc])})


(defn- grep-match?
  "Is the current node a match?  If so, capture it in state using found-match-fn."
  [pattern node state loc breadcrumb found-match-fn]
  (if-not (or (traversable-coll? node) (nil? node))
    (cond
      (and (instance? java.util.regex.Pattern pattern) (re-matches pattern (.toString node))) (found-match-fn state breadcrumb loc)
      (and (string? pattern) (.contains (.toString node) pattern))                            (found-match-fn state breadcrumb loc)
      (and (fn? pattern) (pattern node))                                                      (found-match-fn state breadcrumb loc)
      (= pattern node)                                                                        (found-match-fn state breadcrumb loc)
      :else                                                                                   nil)
    nil))


(defn- grep-tree-visitor
  "Create a visitor callback function--called for every node in the source node's graph.
  This function maintains the state of the breadcrumb vector and accumulates matches
  in the state vector."
  [pattern grep-match-fn found-match-fn]
  (let [breadcrumb (atom [])]
    (fn [node state loc]
      (swap! breadcrumb update-breadcrumb loc)
      (grep-match-fn pattern node state loc @breadcrumb found-match-fn))))


(defn grep
  "Recursively grep the contents of node for elements matching pattern.  Pattern can be
  any type:

  * If pattern is a regular expression, it is matched against (.toString obj).
  * String patterns match any substring of (.toString obj).
  * If pattern is a function then the truthiness of (pattern node) determines matches.
  * All other types must match literally.

  If node is omitted, returns an arity-1 function with pattern bound to the specified value,
  and the remaining parameter being the start node.

  Returns [[breadcrumb1 match1-parent] [breadcrumb2 match2-parent] ... [breadcrumbn matchn-parent]]"
  ([pattern node]
   (:state
    (v/tree-visitor (v/tree-zipper node) [] [(grep-tree-visitor pattern grep-match? found-match-container)])))
  ([pattern]
   (partial grep pattern)))


(defn grep-nodes
  "Recursively grep the contents of node for elements matching pattern.  Pattern can be
  any type:

  * If pattern is a regular expression, it is matched against (.toString obj).
  * String patterns match any substring of (.toString obj).
  * If pattern is a function then the truthiness of (pattern node) determines matches.
  * All other types must match literally.

  If node is omitted, returns an arity-1 function with pattern bound to the specified value,
  and the remaining parameter being the start node.

  Returns [[breadcrumb1 match1-node] [breadcrumb2 match2-node] ... [breadcrumbn matchn-node]]

  where the node objects are the clojure.zip visitor nodes corresponding to the matches.

  Use the functions inside clojure.zip to traverse the object graph from the match node locations
  and to retrieve the value(s) you want to manipulate."
  ([pattern node]
   (:state
    (v/tree-visitor (v/tree-zipper node) [] [(grep-tree-visitor pattern grep-match? found-match-node)])))
  ([pattern]
   (partial grep-nodes pattern)))
