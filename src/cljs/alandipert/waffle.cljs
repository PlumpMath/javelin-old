(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :as    d]
   [cljs.core                :as    cljs])
  (:require-macros
   [alandipert.waffle.macros :refer [with-let]])
  (:refer-clojure :exclude [map]))

;;; Internals

(let [rank  (atom 0)
      stamp (atom 0)]
  (def next-rank  #(swap! rank inc))
  (def next-stamp #(swap! stamp inc)))

(defprotocol INode "Base functionality of either EventStream or
Behavior.  The graph is modeled with objects containing atoms
instead of with just atoms.  This makes it easier for consumers to
extend types used here."
             (-sinks [_] "Atom of a collection of INodes")
             (-rank [_] "Atom of a number")
             (-update-fn [_] "Atom of function to invoke with new pulses."))

(defrecord EventStream [sinks rank update-fn]
  INode
  (-sinks [_] sinks)
  (-rank [_] rank)
  (-update-fn [_] update-fn))

(defrecord Behavior [sinks rank update-fn last-value]
  INode
  (-sinks [_] sinks)
  (-rank [_] rank)
  (-update-fn [_] update-fn)

  cljs.core/IDeref
  (-deref [_] @last-value))

(defn make-pulse
  [value]
  {:stamp (next-stamp) :value value})

(defn make-event-stream
  [sinks update-fn]
  (EventStream. (atom sinks)
                (atom (next-rank))
                (atom update-fn)))

(defn behavior-fn
  "The difference between Behaviors and EventStreams is that behaviors
  only propagate when the new value differs from the Behavior's last
  value.

  update-fn is wrapped with a function returning the ::halt sentinel
  if the value is not new."
  [behavior update-fn]
  (fn [pulse]
    (if (not= (:value pulse) @behavior)
      (@(-update-fn behavior) pulse)
      ::halt)))

(defn make-behavior
  [sinks update-fn init-value]
  (with-let [b (Behavior. (atom sinks)
                            (atom (next-rank))
                            (atom nil)
                            (atom init-value))]
    (reset! (-update-fn b) (behavior-fn b update-fn))))

(defn propagate!
  [node pulse]
  (loop [queue (priority-map {:node node :pulse pulse} @(-rank node))]
    (if (seq queue)
      (let [{:keys [node pulse]} (key (peek queue))
            next-pulse (@(-update-fn node) pulse)]
        (if (not= next-pulse ::halt)
          (recur (reduce #(assoc %1 {:node %2 :pulse next-pulse} @(-rank %2))
                         (pop queue)
                         @(-sinks node))))))))

(defn add-sink!
  [node dependent]
  (swap! (-sinks node) conj dependent)
  (if (> @(-rank node) @(-rank dependent))
    (doseq [dep (d/bf-seq identity (comp deref -sinks) dependent)]
      (reset! (-rank dep) (next-rank)))))

(defn remove-sink!
  [node dependent]
  (swap! (-sinks node) #(filterv (partial not= dependent) %)))

(defn event-stream
  [nodes update-fn]
  (with-let [e (make-event-stream [] update-fn)]
    (doseq [node nodes]
      (add-sink! node e))))

(defn internal
  [depends-on]
  (event-stream depends-on identity))

;;; API

(def receiver internal)

(defn send-event!
  [node value]
  (propagate! node (make-pulse value)))

(defn map
  [e f]
  (event-stream [e] #(update-in % [:value] f)))

;;; Example

(defn doit []
  (let [e1 (receiver)
        e2 (-> e1
               (map #(.toUpperCase %))
               (map #(js/alert %)))]
    (doseq [c "omg"]
      (send-event! e1 c))))