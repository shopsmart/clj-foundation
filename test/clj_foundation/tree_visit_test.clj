(ns clj-foundation.tree-visit-test
  (:require [clojure.zip :as z]
            [clojure.core.reducers :as r]
            [clojure.test :refer :all]
            [clj-foundation.tree-visit :refer :all])
  (:import [java.util Map Map$Entry]))

(def testdata {:a {:a 1 :b 2 :c {:aa 3 :bb 4 :cc {:aaa [1 2 3 4 5] :bbb "blah!"}}}})

(def cursor (tree-zipper testdata))

(defn cursor-seq-inf
	([] (cursor-seq-inf cursor))
	([c] (cons c (lazy-seq (cursor-seq-inf (z/next c))))))

(defn cursor-seq [] (take-while #(not (z/end? %)) (cursor-seq-inf)))

(deftest testDepth_root_returnsZero
  (is (= 0 (depth cursor))))

(deftest testDepth_mapEntry_sameDepthAsParent
  (let [new-loc (z/next cursor)]
    (is (= 0 (depth new-loc)))
    (is (instance? Map$Entry (z/node new-loc)))))

(deftest testDepth_mapKey_level1
  (let [new-loc (-> cursor z/next z/next)]
    (is (= 1 (depth new-loc)))
    (is (= :a (z/node new-loc)))))

(deftest testDepth_mapVal_level1
  (let [new-loc (-> cursor z/next z/next z/next)]
    (is (= 1 (depth new-loc)))
    (is (map? (z/node new-loc)))))

(deftest testDepth_mapEntry_level1
  (let [new-loc (-> cursor z/next z/next z/next z/next)]
    (is (= 1 (depth new-loc)))
    (is (instance? Map$Entry (z/node new-loc)))))

(deftest testDepth_mapKey_level2
  (let [new-loc (-> cursor z/next z/next z/next z/next z/right z/next)]
    (is (= 2 (depth new-loc)))
    (is (= :b (z/node new-loc)))))

(deftest testDepth_mapVal_level2
  (let [new-loc (-> cursor z/next z/next z/next z/next z/right z/next z/next)]
    (is (= 2 (depth new-loc)))
    (is (= 2 (z/node new-loc)))))

(deftest testDepth_find5_level5
  (let [new-loc (first (filter #(= 5 (z/node %)) (cursor-seq)))]
    (is (= 5 (depth new-loc)))))

(deftest testDepth_findBlah_level4
  (let [new-loc (first (filter #(= "blah!" (z/node %)) (cursor-seq)))]
    (is (= 4 (depth new-loc)))))

;---------------------------------------------------------------------------------------------------------

(defn next-b [[breadcrumb cursor]]
  (let [loc (z/next cursor)]
    [(update-breadcrumb breadcrumb loc) loc]))

(deftest test-update-breadcrumb-root
  (is (= [] (update-breadcrumb [] cursor))))

(deftest test-update-breadcrumb-root-next
  (let [[result _] (-> [[] cursor] next-b)]
    (is (= [] result))))

(deftest test-update-breadcrumb-firstchild
  (let [[result _] (-> [[] cursor] next-b next-b)]
    (is (= [:a] result))))

(deftest test-update-breadcrumb-level2
  (let [[result _] (-> [[] cursor] next-b next-b next-b next-b next-b)]
    (is (= [:a :a] result))))

(deftest test-update-breadcrumb-level2b
  (let [[result _] (-> [[] cursor] next-b next-b next-b next-b next-b next-b next-b next-b)]
    (is (= [:a :b] result))))

(deftest test-update-breadcrumb-level3a
  (let [[result _] (-> [[] cursor] next-b next-b next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b)] ;; go to :aa
    (is (= [:a :c :aa] result))))

(deftest test-update-breadcrumb-level3c
  (let [[result _] (-> [[] cursor] next-b next-b next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b)]
    (is (= [:a :c :cc] result))))

(deftest test-update-breadcrumb-level4a
  (let [[result _] (-> [[] cursor] next-b next-b next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b)]
    (is (= [:a :c :cc :aaa] result))))

(deftest test-update-breadcrumb-level5
  (let [[result _] (-> [[] cursor] next-b next-b next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b next-b next-b
                     next-b next-b next-b next-b
                     next-b)]
    (is (= [:a :c :cc :aaa 0] result))))
