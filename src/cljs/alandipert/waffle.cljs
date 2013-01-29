(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :as    d]
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

(defn make-node
  "Idempotently FRP-ize an atom."
  ([atm]
     (make-node atm (constantly true)))
  ([atm update-fn]
     (doto atm
       (alter-meta! update-in [::sinks] #(or % []))
       (alter-meta! update-in [::rank] #(or % (next-rank)))
       (alter-meta! update-in [::update-fn] #(or % update-fn)))))

(defn increase-sink-ranks!
  "Walk source's sinks in rank order and increase the rank of each."
  [source]
  (doseq [dep (d/bf-seq identity (comp ::sinks meta) source)]
    (alter-meta! dep assoc-in [::rank] (next-rank))))

(defn add-propagator!
  "Attaches a watch to atm keyed by sink.  If filter-repeats? is true,
  only new values are propagated."
  [atm sink filter-repeats?]
  (add-watch atm sink (fn [k ref old new]
                        (if filter-repeats?
                          (and (not= old new) (propagate! sink))
                          (propagate! sink)))))

(defn attach!
  "Attaches sink to one or more atoms.  If filter-repeats? is true,
  only new values from the atoms are propagated to sink."
  [atoms sink filter-repeats?]
  (with-let [attached-sink sink]
    (doseq [source (c/map make-node atoms)]
      (alter-meta! source update-in [::sinks] conj sink)
      (if (> (-> source meta ::rank) (-> sink meta ::rank))
        (increase-sink-ranks! source))
      (add-propagator! source sink filter-repeats?))))

(defn lift
  [f]
  (fn [& atoms]
    (let [update #(apply (if (fn? f) f @f) (c/map deref atoms))]
      (with-let [lifted (atom (update))]
        (attach! atoms (make-node lifted #(reset! lifted (update))) true)))))

(defn map
  [source f]
  (with-let [sink (atom nil)]
    (attach! [source] (make-node sink #(reset! sink (f @source))) false)))

;;; Example

(defn doit []
  (let [n1 (atom 0)
        n2 (atom 0)
        sum ((lift +) n1 n2)]
    (map sum #(.write js/document %))
    ;; Meanwhile...
    (.setInterval js/window #(swap! n1 inc) 1000)))