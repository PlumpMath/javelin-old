(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :as    d])
  (:require-macros
   [alandipert.waffle.macros :refer [with with-let]]))

(declare reset-cell! attach! detach!)

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def none
  "Value indicating that there is no value to return."
  ::none)

(defn atom?
  "True if obj is a ClojureScript atom."
  [obj]
  (instance? cljs.core/Atom obj))

(defn cell?
  "True if obj is a ClojureScript atom marked with ::cell metadata."
  [obj]
  (and (atom? obj) (-> obj meta ::cell)))

(defn changes?
  "Is this cell only propagating when the value changes?"
  [cell]
  (-> cell meta ::changes))

(defn done?
  "Is this cell marked as done?"
  [cell]
  (-> cell meta ::done))

;; INTERNAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [rank (atom 0)]
  (def next-rank
    "Get the next item in a monotonically increasing sequence of integers."
    #(swap! rank inc)))

(def swapping (atom ::not-swapping))

(defn- deref*
  "Dereferences obj if it satisfies IDeref, otherwise returns obj."
  [obj]
  (if (satisfies? cljs.core/IDeref obj) @obj obj))

(defn- input-cell
  "Idempotently mutate the atom atm, adding metadata that makes it a cell.
  The pieces of metadata that are added are:

  ::cell     - Marker identifying this atom as a cell.
  ::rank     - Numeric ranking that determines evaluation order.
  ::sinks    - Vector of dependent cells.
  ::done     - True when this cell should not participate in evaluation.
  ::silent   - True when this cell should not propagate to its sinks.
  ::thunk    - Thunk to be invoked on evaluation that may return ::none
  and stop propagation to its sinks."
  ([atm]
   (input-cell atm nil))
  ([atm thunk]
   (let [commit! #(let [srcs (-> atm meta ::sources)]
                    (if (seq srcs)
                      (reset-cell! atm (deref* (first srcs)))
                      nil))]
     (doto atm
       (alter-meta!
         merge
         {::cell     true
          ::rank     (next-rank) 
          ::sources  []
          ::sinks    #{} 
          ::done     false 
          ::silent   false 
          ::thunk    (or thunk commit!)})
       detach!))))

(defn- increase-sink-ranks!
  "Preorder traversal of tree rooted at source cell, increasing the
  rank of all descendents. This ensures that all sinks have higher
  ranks than their sources---a necessary invariant for glitch
  elimination."
  [source]
  (doseq [dep (d/bf-seq identity (comp ::sinks meta) source)]
    (alter-meta! dep assoc-in [::rank] (next-rank))))

(defn- propagate!
  "Initiate the FRP evaluation process. The atom atm must be an input cell.
  Updates are propagated through the dependency graph in rank order."
  [cell]
  (loop [queue (priority-map cell (-> cell meta ::rank))]
    (when (seq queue)
      (let [cell      (key (peek queue))
            siblings  (pop queue)
            q-add     #(assoc %1 %2 (-> %2 meta ::rank))
            halt?     #(or (= ::none ((-> cell meta ::thunk)))
                           (-> cell meta ::silent))
            children  (-> cell meta ::sinks)]
        (if (and (seq children) (every? done? children))
          (detach! cell)
          (recur (if-not (halt?)
                   (reduce q-add siblings (remove done? children))
                   siblings)))))))

(defn reset-cell!
  "Reset the contents of a cell without triggering the validator exception."
  [cell value]
  (with value
    (when (not= ::none value)
      (->> value (reset! swapping) (reset! cell)) 
      (reset! swapping ::not-swapping))))

;; CREATE CELL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn input
  "FRP-ize an atom, making it an input cell. Input cells accept arbitrary
  values and are manipulated using the normal swap! and reset!."
  [atm]
  {:pre [(not (cell? atm))]}
  (input-cell atm))

(defn lift
  "Given a function f or a cell containing a function f, returns a function
  that returns a formula cell whose value is always f applied to the derefed
  values of its arguments. Formula cells cannot be updated directly using
  swap! or reset!."
  [f]
  (fn [& args]
    (let [eval      #(apply (deref* f) (map deref* %))
          lifted    (atom (eval args))
          thunk     #(with-let [value (eval (-> lifted meta ::sources))]
                       (reset-cell! lifted value))
          cell-args (filter cell? args)]
      (->> thunk (input-cell lifted) (attach! args))
      (if (or (empty? cell-args) (every? changes? cell-args))
        (alter-meta! lifted assoc-in [::changes] true)) 
      lifted)))

;; CONTROL PROPAGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn done!
  "Mark this cell as done."
  [cell]
  {:pre [(cell? cell)]}
  (doto cell (alter-meta! assoc-in [::done] true)))

(defn detach!
  "Detaches this cell from its sources and makes it an input cell."
  [cell]
  {:pre [(cell? cell)]}
  (let [watch-fn    (fn [_ cell _ _]
                      (if-not (done? cell)
                        (propagate! cell)))
        remove-this (fn [source]
                      (-> source
                        (alter-meta!
                          update-in [::sinks]
                          (fn [sinks]
                            (remove #(= cell %) sinks)))))]
    (mapv remove-this (-> cell meta ::sources))
    (doto cell
      (alter-meta! assoc-in [::sources] [])
      (add-watch ::propagate watch-fn)
      (set-validator! nil))))

(defn attach!
  "Attaches a sink cell to one or more source cells and makes it a formula
  cell. Attaching a sink to a source sets up the dependency graph."
  [sources sink]
  {:pre [(and (cell? sink) (empty? (-> sink meta ::sources)))]}
  (with (doto sink
          (alter-meta! assoc-in [::sources] (vec sources))
          (remove-watch ::propagate)
          (set-validator! #(and (not= ::not-swapping %) (= @swapping %))))
    (doseq [source sources]
      (alter-meta! source update-in [::sinks] conj sink)
      (if (> (-> source meta ::rank) (-> sink meta ::rank))
        (increase-sink-ranks! source)))))

(defn changes
  "Given a cell, returns a cell which only propagates pulses that changed
  the value of the given cell."
  [cell]
  {:pre [(cell? cell)]}
  (if (or (not (cell? cell)) (changes? cell)) 
    cell
    (let [previous (atom ::none)
          update   (fn [value]
                     (if (not= value @previous)
                       (reset! previous value)
                       ::none))]
      (doto ((lift update) cell)
        (alter-meta! assoc-in [::changes] true)))))

(defn silence
  "Given a cell, returns a cell which is updated during propagation but does
  not propagate pulses to their sinks."
  [cell]
  {:pre [(cell? cell)]}
  (doto ((lift identity) cell)
    (alter-meta! assoc-in [::silent] true)))

