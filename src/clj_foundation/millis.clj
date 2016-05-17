(ns clj-foundation.millis
  "Convert various time values to milliseconds and back"
  (:require [schema.core :as s :refer [=> =>*]]
            [etlsupport.patterns :refer [let-map]]
            [etlsupport.math :refer [->MixedNumber]])
  (:import [etlsupport.math IMixedNumber])
  (:gen-class))

(defn <-seconds
  "Seconds to milliseconds"
  [s]
  (* s 1000))

(defn <-minutes
  "Minutes to milliseconds"
  [m]
  (->> m
    (* 60)
    (<-seconds)))

(defn <-hours
  "Hours to milliseconds"
  [h]
  (->> h
    (* 60)
    (<-minutes)))

(defn <-days
  "Days to milliseconds"
  [d]
  (->> d
    (* 24)
    (<-hours)))

(defn ->seconds
  "Milliseconds to seconds"
  [m]
  (-> m (/ 1000) ->MixedNumber))

(defn ->minutes
  "Milliseconds to minutes"
  [m]
  (-> m ->seconds .number
      (/ 60) ->MixedNumber))

(defn ->hours
  "Milliseconds to hours"
  [m]
  (-> m ->minutes .number
      (/ 60) ->MixedNumber))

(defn ->days
  "Milliseconds to days"
  [m]
  (-> m ->hours .number
      (/ 24) ->MixedNumber))

(defrecord dhms [millis]
  IMixedNumber
  (parts [this]
    (let-map [millis (:millis this)
              days (->days millis)
              hours (->hours (* (:frac (.parts days)) 24 60 60 1000))
              minutes (->minutes (* (:frac (.parts hours)) 60 60 1000))
              seconds (->seconds (* (:frac (.parts days)) 60 1000))]))

  (toString [this]
    (let [parts (.parts this)]
      (apply str
             (map (fn [part]
                    (let [whole (:whole (.parts (part parts)))]
                      (if (> whole 0)
                        (str whole (second (str part)) " ")
                        "")))
                  [:days :hours :minutes :seconds])))))
