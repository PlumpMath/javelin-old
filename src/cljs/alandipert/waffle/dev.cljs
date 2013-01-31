(ns alandipert.waffle.dev
  (:require [alandipert.waffle   :as w]
            [alandipert.waffle.e :as e]))

(set! cljs.core/*print-meta* true)

(def log #(js/console.log (clj->js %)))

(defn pr-cell
  [cell]
  (log {:v @cell
        :r (-> cell meta :alandipert.waffle/rank)
        :s (-> cell meta :alandipert.waffle/sinks)
        :f (-> cell meta :alandipert.waffle/thunk)}))

(defn pr** [& cells] (mapv pr-cell cells))

(def timer*     #(.setInterval js/window (constantly true) %))
(def identity*  (w/lift identity))
(def inc*       (w/lift inc))
(def pr*        (w/lift log))
(def odd?*      (w/lift odd?))

(defn doit []
  (let [in (w/input (atom 0))]
    (pr* (w/changes in))
    (.setInterval js/window #(swap! in identity) 1000)))
