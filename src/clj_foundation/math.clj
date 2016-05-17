(ns clj-foundation.math
  "Various math support helpers"
  (require [clj-foundation.patterns :refer [let-map]])
  (:gen-class))


(defprotocol INumberParts
  (parts [this]))

(deftype MixedNumber [number]
  INumberParts
  (parts [this]
    (let [r (rationalize number)
          n (numerator r)
          d (denominator r)]
      (if (> n d)
        (let-map
            [whole (long (/ n d))
             frac (/ (- n (* whole d)) d)])
        {:whole 0
         :frac (/ n d)})))

  (toString [this]
    (str (:whole (.parts this)) " " (:frac (.parts this)))))

;; (compile 'clj-foundation.math)
