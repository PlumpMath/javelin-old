(ns alandipert.waffle.dev
  (:require [alandipert.waffle   :as w]
            [alandipert.waffle.e :as e]))

(defn doit []
  (let [in (w/input (atom 0))]
    ((w/lift #(js/console.log %)) in)
    (.setInterval js/window #(swap! in inc) 1000)))
