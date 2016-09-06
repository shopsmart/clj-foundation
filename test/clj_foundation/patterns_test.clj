(ns clj-foundation.patterns-test
  (:require [clojure.test :refer :all]
            [schema.core :as s :refer [=> =>*]]
            [clj-foundation.patterns :refer :all]))


(s/set-fn-validation! true)


;; Schema/type tests ---------------------------------------------------------------------------------------

(deftest get-package-test
  (testing "get-package returns java.lang given java.lang.Object"
    (is (= "java.lang" (get-package Object))))

  (testing "Passing something other than a Class throws an exception"
    (is (thrown? RuntimeException (get-package "Object")))))

(deftest get-class-name-test
  (testing "get-class-name returns Object given java.lang.Object"
    (is (= "Object" (get-class-name Object)))))

;; Singleton -----------------------------------------------------------------------------------------------

(def invocations (atom 0))

(def-singleton-fn ct (swap! invocations inc))

(deftest singleton-test
  (testing "(def-singleton-fn name & body) A fn that evaluates body once then always returns that result."
    (is (= 1 (ct)))
    (is (= 1 (ct)))
    (is (= 1 @invocations))))

;; Nothingness ---------------------------------------------------------------------------------------------

(deftest nothingness-test
  (testing "An explicit value for Nothing so one can write: (s/defn foo :- (s/types s/Str Nothing) [] ....)"

    (testing "mapcat fns returning nothing add nothing to the mapcat result"
      (is (= '(0 2 4 6) (mapcat (fn [x] (if (even? x) [x] nothing)) (range 7)))))

    (testing "cons something into nothing results in a list of something"
      (is (= '(:something) (cons :something nothing))))

    (testing "assoc a pair into nothing results in a map containing the k,v pair"
      (is (= {:something "another thing"} (assoc nothing :something "another thing"))))

    (testing "conj vector pairs into nothing results in a map containing the k,v pairs"
      (is (= {:something "some thing" :another-thing "and more"}
             (conj nothing [:something "some thing"] [:another-thing "and more"]))))

    (testing "Nothing is not something."
      (is (nil? (something? nothing))))

    (testing "Something is not nothing"
      (is (something? "something"))
      (is (= "something" (something? "something")))
      (is (not= nothing (something? nothing)))
      (is (nil? (something? nothing))))))


;; let-map -------------------------------------------------------------------------------------------------

(deftest let-map-test
  (testing "let-map returns a map of the let assignments"
    (is (= {:a 3.14 :b 42}
           (let-map [a 3.14
                     b 42]))))

  (testing "let-map allows subsequent variables to refer to prior ones"
    (is (= {:first 2 :double 4}
           (let-map [first 2
                     double (* first first)]))))

  (testing "let-map returns [{map-values} let-result] when let-result isn't a map"
    (is (= [{:first 2 :double 4} 42]
           (let-map [first 2
                     double (* first first)]
             42))))

  (testing "let-map merges a result map into the let-map if the result is a map"
    (is (= {:first 4 :double 4 :triple 6}
           (let-map [first 2
                     double (* first first)]
             {:first 4 :triple 6})))))

;; letfn-map ------------------------------------------------------------------------------------------------

(deftest letfn-map-test
  (testing "letfn-map returns a map of its functions"
    (is (= 32
           (let [map (letfn-map [(f [x] (* 2 x))])
                 d (:f map)]
             (d 16))))))


(run-tests)
