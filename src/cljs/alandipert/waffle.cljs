(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :as    d])
  (:require-macros
   [alandipert.waffle.macros :refer [with-let]]))

(let [rank (atom 0)]
  (def next-rank
    "Get the next item in a monotonically increasing sequence of integers."
    #(swap! rank inc)))

(def swapping (atom ::not-swapping))

(defn make-input-cell
  "Idempotently mutate the atom atm, adding metadata that makes it a cell."
  ([atm]
     (make-input-cell atm (constantly true)))
  ([atm update-fn]
     (doto atm
       (alter-meta! update-in [::sinks] #(or % []))
       (alter-meta! update-in [::rank] #(or % (next-rank)))
       (alter-meta! update-in [::thunk] #(or % update-fn)))))

(defn make-formula-cell
  "Make an input cell and add a validator function to effectively disable
  swap! and reset!, as the values in formula cells are dictated by the
  dependency graph and should not be arbitrarily updated."
  [& args]
  (with-let [cell (apply make-input-cell args)]
    (set-validator! cell #(and (not= ::not-swapping %) (= @swapping %)))))

(defn increase-sink-ranks!
  "Preorder traversal of tree rooted at source, increasing the rank of
  all descendents. This ensures that all sinks have higher ranks than
  their sources---a necessary invariant for glitch elimination."
  [source]
  (doseq [dep (d/bf-seq identity (comp ::sinks meta) source)]
    (alter-meta! dep assoc-in [::rank] (next-rank))))

(defn attach!
  "Attaches a sink cell to one or more source cells. This creates the
  dependency graph that is used for propagation."
  [sources sink]
  (with-let [attached-sink sink]
    (doseq [source (map make-input-cell sources)]
      (alter-meta! source update-in [::sinks] conj sink)
      (if (> (-> source meta ::rank) (-> sink meta ::rank))
        (increase-sink-ranks! source)))))

(defn propagate!
  "Initiate the FRP evaluation process. The atom atm must be an input cell.
  Updates are propagated through the dependency graph in rank order."
  [atm]
  (loop [queue (priority-map atm (-> atm meta ::rank))]
    (when (seq queue)
      (let [node (key (peek queue))
            remainder (pop queue)]
        (recur (if (not= ::halt ((-> node meta ::thunk)))
                 (reduce #(assoc %1 %2 (-> %2 meta ::rank))
                         remainder
                         (-> node meta ::sinks))
                 remainder))))))

(defn input
  "FRP-ize an atom, making it an input cell. Input cells accept arbitrary
  values and are manipulated using the normal swap! and reset!."
  [atm]
  (if (-> atm meta ::sinks)
    (throw (js/Error. "Atom is already an FRP cell!"))
    (doto atm
      (add-watch ::propagate (fn [_ atm _ _] (propagate! atm)))
      make-input-cell)))

(defn lift
  "Given a function f or a cell containing a function f, returns a function
  that returns a formula cell whose value is always f applied to the derefed
  values of its arguments. Formula cells cannot be updated directly using
  swap! or reset!."
  [f]
  (fn [& atoms]
    (let [update #(apply (if (fn? f) f @f) (map deref atoms))]
      (with-let [lifted (atom (update))]
        (->> #(do (->> (update) (reset! swapping) (reset! lifted))
                (reset! swapping ::not-swapping)) 
             (make-formula-cell lifted)
             (attach! atoms))))))

