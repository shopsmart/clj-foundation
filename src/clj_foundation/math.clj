(ns clj-foundation.math
  "Various math support helpers"
  (require [etlsupport.patterns :refer [let-map]])
  (:gen-class))


(definterface IMixedNumber
  (parts []))

(deftype MixedNumber [number]
  IMixedNumber
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
