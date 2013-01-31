(ns micha
  (:require [tailrecursion.javelin   :as w]
            [tailrecursion.javelin.e :as e]))

(set! cljs.core/*print-meta* true)

(def log #(js/console.log (clj->js %)))

(defn pr-cell
  [cell]
  (log {:v @cell
        :r (-> cell meta :tailrecursion.javelin/rank)
        :s (-> cell meta :tailrecursion.javelin/sinks)
        :f (-> cell meta :tailrecursion.javelin/thunk)}))

(defn pr** [& cells] (mapv pr-cell cells))

(def timer*     #(.setInterval js/window (constantly true) %))
(def identity*  (w/lift identity))
(def inc*       (w/lift inc))
(def pr*        (w/lift log))
(def odd?*      (w/lift odd?))

(defn reattach!
  [sink & sources]
  (w/detach! sink)
  (w/attach! sources sink))

(defn doit []
  (let [in    (w/input (atom 0))
        in2   (w/input (atom 0))
        outs  [(identity* in2) (odd?* in2)] 
        out   (pr* (nth outs 1))
        swap  #(reattach! out (nth outs (mod @in 2)))]
    (reattach! in2 in)
    (.setInterval js/window #(swap! in inc) 1000)
    (.setInterval js/window swap 3000)))
