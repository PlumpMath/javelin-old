(ns alandipert.waffle.dev
  (:require [alandipert.waffle   :refer [input]]
            [alandipert.waffle.e :as e]))

(defn doit []
  (let [in (input (atom 0))]
    (map #(.write js/document) in)
    (.setInterval js/window #(reset! input inc) 1000)))