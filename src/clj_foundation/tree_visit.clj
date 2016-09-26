(ns clj-foundation.tree-visit
  "Visitor pattern implementation for arbitrary Clojure data structures."
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [clj-foundation.patterns :as p])
  (:import [clojure.lang IPersistentSet IPersistentList IPersistentVector IPersistentMap ISeq]
           [java.util Map Map$Entry])
  (:gen-class))

;;---------------------------------------------------------------------------------
;; Thanks to: http://www.ibm.com/developerworks/library/j-treevisit/

(defmulti tree-branch? class)
(defmethod tree-branch? :default [_] false)
(defmethod tree-branch? IPersistentVector [v] true)
(defmethod tree-branch? IPersistentMap [m] true)
(defmethod tree-branch? IPersistentList [l] true)
(defmethod tree-branch? IPersistentSet [s] true)
(defmethod tree-branch? ISeq [s] true)

(defmulti tree-children class)
(defmethod tree-children IPersistentVector [v] v)
(defmethod tree-children IPersistentMap [m] (seq m))
(defmethod tree-children IPersistentList [l] l)
(defmethod tree-children IPersistentSet [s] (seq s))
(defmethod tree-children ISeq [s] s)

(defmulti tree-make-node (fn [node children] (class node)))
(defmethod tree-make-node IPersistentVector [v children] (vec children))
(defmethod tree-make-node IPersistentMap [m children]
           (apply hash-map (apply concat children)))
(defmethod tree-make-node IPersistentList [_ children] children)
(defmethod tree-make-node IPersistentSet [_ children] (apply set children))
(defmethod tree-make-node ISeq [node children] (apply list children))
(prefer-method tree-make-node IPersistentList ISeq)


(defn tree-zipper [node]
  (zip/zipper tree-branch? tree-children tree-make-node node))


(defn visit-node
  [start-node start-state visitors loc]
  (loop [node start-node
         state start-state
         [first-visitor & rest-visitors] visitors]
    (let [context (merge {:node node, :state state, :stop false, :next false}
                         (first-visitor node state loc))
          {new-node :node
           new-state :state
           :keys (stop next)} context]
      (if (or next stop (nil? rest-visitors))
        {:node new-node, :state new-state, :stop stop}
        (recur new-node new-state rest-visitors)))))


(defn tree-visitor
  ([zipper visitors]
     (tree-visitor zipper nil visitors))
  ([zipper initial-state visitors]
     (loop [loc zipper
            state initial-state]
       (let [context (visit-node (zip/node loc) state visitors loc)
             new-node (:node context)
             new-state (:state context)
             stop (:stop context)
             new-loc (if (= new-node (zip/node loc))
                       loc
                       (zip/replace loc new-node))
             next-loc (zip/next new-loc)]
         (if (or (zip/end? next-loc) (= stop true))
           {:node (zip/root new-loc) :state new-state}
           (recur next-loc new-state))))))


(defn traversable-coll?
  "Is this a kind of collection we can traverse?"
  [node]
  (or (map? node) (vector? node) (list? node) (set? node) (seq? node)))


(defn- result-coll?
  "Don't return Map$Entrys as results; other collection types are supported."
  [node]
  (and (not (instance? Map$Entry node))
       (traversable-coll? node)))


(defn root-node?
  "Is the cursor at the root of the collection?"
  [n]
  (nil? (zip/up n)))                    ; (nil? is redundant but explicit)


(defn parent-container
  "Return's the current node's parent container.  If the current node is the
  root node, returns the current node."
  [loc]
  (let [parent-result (some-> loc zip/up zip/node)]
    (cond
      (root-node? loc)             (zip/node loc)
      (result-coll? parent-result) parent-result
      :else                        (parent-container (zip/up loc)))))



(defn node-at
  "Return the data node starting at cursor after applying zip/traversal-fns.
  Returns patterns/NO-RESULT-ERROR if there is no node at the specified position."
  [loc & traversal-fns]
  (let [new-loc (reduce (fn [l t] (some-> l t)) loc traversal-fns)]
    (if new-loc
      (zip/node new-loc)
      p/NO-RESULT-ERROR)))


(defn depth
  "Return the number of traversals required to reach the current node starting
  from the root node."
  [loc]
  (if (root-node? loc)
    0
    (+ (if (instance? Map$Entry (zip/node loc))
         0
         1)
       (depth (zip/up loc)))))


(defn index
  "Return the 0-based position of the current element inside its collection.
  This value is only meaningful if the current collection is sequential."
  [loc]
  (count (zip/lefts loc)))



(defn update-breadcrumb
  "A breadcrumb vector contains the human-readable path from the root object to
  the current one in a tree-visit session.

  Starting with [] representing the root object in a Clojure nested graph, to
  maintain the breadcrumb vector, after traversing to a new loc with a call to
  (zip/next cursor) or similar, call (update-breadcrumb old-breadcrumb new-loc)
  to obtain a new breadcrumb vector corresponding with the new location."
  [old-path loc]
  (let [parent (node-at loc zip/up)
        new-depth (depth loc)
        new-index (index loc)
        new-node (zip/node loc)]
    (if (or (instance? Map$Entry new-node) (root-node? loc))
      old-path
      (if (instance? Map$Entry parent)
        (if-not (traversable-coll? new-node)
          (conj (vec (take (dec new-depth) old-path)) new-node)
          old-path)
        (conj (vec (take (dec new-depth) old-path)) new-index)))))
