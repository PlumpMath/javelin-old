(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :as    d])
  (:require-macros
   [alandipert.waffle.macros :refer [with with-let]]))

(let [rank (atom 0)]
  (def next-rank
    "Get the next item in a monotonically increasing sequence of integers."
    #(swap! rank inc)))

(def none ::none)

(def swapping (atom ::not-swapping))

(defn atom?
  "True if obj is a ClojureScript atom."
  [obj]
  (instance? cljs.core/Atom obj))

(defn deref*
  "Dereferences obj if it satisfies IDeref, otherwise returns obj."
  [obj]
  (if (satisfies? cljs.core/IDeref obj) @obj obj))

(defn cell?
  "True if obj is a ClojureScript atom marked with ::cell metadata."
  [obj]
  (and (atom? obj) (-> obj meta ::cell)))

(defn detached?
  "Is this cell marked as detached?"
  [cell]
  (-> cell meta ::detached))

(defn changes?
  "Is this cell only propagating when the value changes?"
  [cell]
  (-> cell meta ::changes))

(defn detach!
  "Mark this cell as detached."
  [cell]
  (doto cell (alter-meta! assoc-in [::detached] true)))

(defn make-input-cell
  "Idempotently mutate the atom atm, adding metadata that makes it a cell.
  The pieces of metadata that are added are:

  ::cell     - Marker identifying this atom as a cell.
  ::rank     - Numeric ranking that determines evaluation order.
  ::sinks    - Vector of dependent cells.
  ::detached - True when this cell should not participate in evaluation.
  ::silent   - True when this cell should not propagate to its sinks.
  ::thunk    - Thunk to be invoked on evaluation that may return ::none
               and stop propagation to its sinks."
  ([atm]
     (make-input-cell atm (constantly true)))
  ([atm thunk]
   (doto atm (alter-meta! merge {::cell     true
                                 ::rank     (next-rank)
                                 ::sinks    []
                                 ::detached false
                                 ::silent   false
                                 ::thunk    thunk}))))

(defn make-formula-cell
  "Make an input cell and add a validator function to effectively disable
  swap! and reset!, as the values in formula cells are dictated by the
  dependency graph and should not be arbitrarily updated."
  [& args]
  (with-let [cell (apply make-input-cell args)]
    (set-validator! cell #(and (not= ::not-swapping %) (= @swapping %)))))

(defn increase-sink-ranks!
  "Preorder traversal of tree rooted at source cell, increasing the
  rank of all descendents. This ensures that all sinks have higher
  ranks than their sources---a necessary invariant for glitch
  elimination."
  [source]
  (doseq [dep (d/bf-seq identity (comp ::sinks meta) source)]
    (alter-meta! dep assoc-in [::rank] (next-rank))))

(defn attach!
  "Attaches a sink cell to one or more source cells. This creates the
  dependency graph that is used for propagation."
  [sources sink]
  (with sink
    (doseq [source sources]
      (alter-meta! source update-in [::sinks] conj sink)
      (if (> (-> source meta ::rank) (-> sink meta ::rank))
        (increase-sink-ranks! source)))))

(defn propagate!
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
        (if (and (seq children) (every? detached? children))
          (detach! cell)
          (recur (if-not (halt?)
                   (reduce q-add siblings (remove detached? children))
                   siblings)))))))

(defn input
  "FRP-ize an atom, making it an input cell. Input cells accept arbitrary
  values and are manipulated using the normal swap! and reset!."
  [atm]
  {:pre [(not (cell? atm))]}
  (let [watch-fn (fn [_ cell _ _]
                   (if-not (detached? cell)
                     (propagate! cell)))]
    (doto atm (add-watch ::propagate watch-fn) make-input-cell)))

(defn lift
  "Given a function f or a cell containing a function f, returns a function
  that returns a formula cell whose value is always f applied to the derefed
  values of its arguments. Formula cells cannot be updated directly using
  swap! or reset!."
  [f]
  (fn [& args]
    (let [eval      #(apply (deref* f) (map deref* args))
          lifted    (atom (eval))
          commit!   #(do (->> % (reset! swapping) (reset! lifted))
                       (reset! swapping ::not-swapping))
          thunk     #(with-let [value (eval)]
                       (if (not= ::none value) (commit! value)))
          cell-args (filter cell? args)]
      (->> thunk (make-formula-cell lifted) (attach! args))
      (if (or (empty? cell-args) (every? changes? cell-args))
        (alter-meta! lifted assoc-in [::changes] true)) 
      lifted)))

(defn changes
  "Given a cell, returns a cell which only propagates pulses that changed
  the value of the given cell."
  [cell]
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
  "Cells that have been silenced get updated during propagation but do not
  propagate pulses to their sinks."
  [cell]
  (doto ((lift identity) cell)
    (alter-meta! assoc-in [::silent] true)))

