(ns alandipert.waffle
  (:require
   [alandipert.priority-map  :refer [priority-map]]
   [alandipert.desiderata    :as    d]
   [cljs.core                :as    c])
  (:require-macros
   [alandipert.waffle.macros :refer [with-let]]))

(let [rank (atom 0)]
  (def next-rank
    "Get the next item in a monotonically increasing sequence of integers."
    #(swap! rank inc)))

(defn make-node
  "Idempotently FRP-ize an atom atm."
  ([atm]
     (make-node atm (constantly true)))
  ([atm update-fn]
     (doto atm
       (alter-meta! update-in [::sinks] #(or % []))
       (alter-meta! update-in [::rank] #(or % (next-rank)))
       (alter-meta! update-in [::thunk] #(or % update-fn)))))

(defn wrap-thunk!
  "Wrap the atom atm's update function with the function f."
  [atm f]
  (let [old-thunk (-> atm meta ::thunk)
        new-thunk #(f old-thunk)]
    (doto atm
      (alter-meta! update-in [::thunk] new-thunk))))

(defn increase-sink-ranks!
  "Walk source's sinks in rank order and increase the rank of each."
  [source]
  (doseq [dep (d/bf-seq identity (comp ::sinks meta) source)]
    (alter-meta! dep assoc-in [::rank] (next-rank))))

(defn attach!
  "Attaches sink to one or more atoms."
  [atoms sink]
  (with-let [attached-sink sink]
    (doseq [source (map make-node atoms)]
      (alter-meta! source update-in [::sinks] conj sink)
      (if (> (-> source meta ::rank) (-> sink meta ::rank))
        (increase-sink-ranks! source)))))

(defn propagate!
  "Initiate the FRP evaluation process. The atom atm must be an input cell.
  Updates are propagated through the dependency graph in topological order."
  [atm]
  (loop [queue (priority-map atm (-> atm meta ::rank))]
    (when (seq queue)
      (let [node (key (peek queue))
            remainder (pop queue)]
        (recur (if (not= ::halt ((-> node meta ::thunk)))
                 (reduce #(assoc %1 %2 (-> %2 meta ::rank)) remainder (-> node meta ::sinks))
                 remainder))))))

(defn input
  "FRP-ize an atom, making it an 'input cell'."
  [atm]
  (if (-> atm meta ::sinks)
    (throw (js/Error. "Atom is already an FRP cell!"))
    (doto atm
      (add-watch ::propagate (fn [_ atm _ _] (propagate! atm)))
      make-node)))

(defn lift
  "Given a function f or an atom containing a function f, returns a function
  that returns an FRP-ized atom whose value is always f applied to the derefed
  values of its arguments."
  [f]
  (fn [& atoms]
    (let [update #(apply (if (fn? f) f @f) (map deref atoms))]
      (with-let [lifted (atom (update))]
        (attach! atoms (make-node lifted #(reset! lifted (update))))))))

;;; Example

(defn doit []
  (let [n1 (input (atom 0))
        n2 (atom 0)
        sum ((lift +) n1 n2)]
    (map (map sum identity) #(.write js/document %))
    ;; Meanwhile...
    (.setInterval js/window #(swap! n1 inc) 1000)))
