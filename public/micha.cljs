(ns micha
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
  (let [last  (atom 0)
        in    (w/input (atom 0))
        outs  [(identity* in) (odd?* in)] 
        out   (pr* (nth outs @last))
        swap  #(do
                 (w/detach! out)
                 (w/attach! [(nth outs (mod (swap! last inc) 2))] out))]
    (.setInterval js/window #(swap! in inc) 1000)
    (.setInterval js/window swap 2000)))
