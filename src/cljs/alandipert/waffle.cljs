(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :refer [bf-seq]]
   [cljs.core                :as    c])
  (:require-macros
   [alandipert.waffle.macros :refer [with-let]])
  (:refer-clojure :exclude [map]))

(defn propagate!
  [atm]
  (loop [queue (priority-map atm (-> atm meta ::rank))]
    (when (seq queue)
      (let [node (key (peek queue))
            remainder (pop queue)]
        (recur (if (not= ::halt ((-> node meta ::update-fn)))
                 (reduce #(assoc %1 %2 (-> %2 meta ::rank)) remainder (-> node meta ::sinks))
                 remainder))))))

(let [rank (atom 0)]
  (def next-rank #(swap! rank inc)))

(defn attach!
  [source sink]
  (alter-meta! source update-in [::sinks] (fnil conj []) sink)
  (if (> (-> source meta ::rank) (-> sink meta ::rank))
    (doseq [dep (bf-seq identity (comp ::sinks meta) source)]
      (alter-meta! dep assoc-in [::rank] (next-rank)))))

(defn make-node!
  [atm update-fn]
  (doto atm
    (alter-meta! assoc
                 ::rank (next-rank)
                 ::sinks []
                 ::update-fn update-fn)
    (add-watch ::propagate (fn [& _] (propagate! atm)))))

(defn behavior
  [init]
  (make-node! (atom init) (constantly true)))

(defn map
  [source f]
  (with-let [sink (atom nil)]
    (make-node! sink #(f @source))
    (attach! source sink)))

(defn lift
  [f]
  (fn [& atoms]
    (let [update #(apply f (c/map deref atoms))]
      (with-let [lifted (atom (update))]
        (make-node! lifted (comp (partial reset! lifted) update))
        (doseq [a atoms] (attach! a lifted))))))

;;; Example

(defn doit []
  (let [n1 (behavior 0)
        n2 (behavior 0)
        sum ((lift +) n1 n2)]
    (map sum #(.log js/console (str "new sum: " %)))
    (.log js/console (str "starting sum: " @sum))

    (doseq [s (bf-seq identity (comp ::sinks meta) n1)]
      (.log js/console (pr-str (meta s))))
;    (js/setInterval #(swap! n1 inc) 300)
;    (js/setInterval #(swap! n2 inc) 500)
    ))