(ns alandipert.waffle
  (:require [alandipert.priority-map :refer [priority-map]]))

(let [rank (atom 0)
      stamp (atom 0)]
  (defn next-rank [] (swap! rank inc))
  (defn next-stamp [] (swap! stamp inc)))

(defn process-queue
  [queue]
  (when (seq queue)
    (let [qv (key (peek queue))            
          next-pulse ((get-in qv [:n :updater]) (:v qv))]
      (if (not= next-pulse ::do-not-propagate)
        (recur (reduce #(assoc %1 {:n %2, :v next-pulse} @(:rank %2))
                       (pop queue)
                       @(get-in qv [:n :sends-to])))))))

(defn propagate-pulse
  [pulse node]
  (process-queue (priority-map {:n node, :v pulse} @(:rank node))))

(defn pulse [stamp value]
  {:stamp stamp, :value value})

(defn attach-listener! [node dependent]
  (swap! (:sends-to node) conj dependent)
  (if (> @(:rank node) @(:rank dependent))
    (loop [q [dependent]]
      (when-let [[cur] dependent]
        (reset! (:rank cur) (next-rank))
        (recur (concat q (:sends-to cur)))))))

(defn event-stream
  [nodes updater]
  (let [this {:updater updater
              :sends-to (atom [])
              :rank (atom (next-rank))}]
    (doseq [node nodes] (attach-listener! node this))
    this))

(defn remove-listener! [node dependent]
  (let [n-before (count @(:sends-to node))
        n-after (count (swap! (:sends-to node)
                              #(filterv (partial not= dependent) %)))
        found-sending (not= n-before n-after)]
    found-sending))

(defn internalE
  ([] (internalE []))
  ([depends-on] (event-stream depends-on identity)))

(defn receiverE []
  (let [evt (internalE)]
    (assoc evt :send-event #(propagate-pulse (pulse (next-stamp) %) evt))))

(defn send-event [node value]
  (propagate-pulse (pulse (next-stamp) value) node))

(defn zeroE
  "Create an event stream that never fires any events."
  []
  (event-stream
   []
   #(throw (js/Error. (str "zeroE : received a value; zeroE should not receive a value; the value was " (pr-str %))))))

(defn oneE
  "Create an event stream that fires just one event with the value val."
  [val]
  (let [sent? (atom false)
        evt (event-stream
             []
             #(if @sent?
               (throw (js/Error. "oneE : received an extra value."))
               (do (swap! sent? not) %)))]
    (.setTimeout js/window #(send-event evt val) 0)
    evt))

(defn mergeE
  "Triggers when any of the argument event stream trigger; carries the
  signal from the last event stream that triggered."
  [& es]
  (if (seq es) (internalE es) (zeroE)))

(defn constantE
  "Transforms this event stream to produce only constant-value."
  [e constant-value]
  (event-stream [e] #(assoc % :value constant-value)))

(defn behavior
  ([e init]
     (behavior e init identity))
  ([e init updater]
     (let [last (atom init)
           underlying #(assoc % :value (reset! last (updater (:value %))))]
       {:last last
        :underlying-raw e
        :underlying underlying})))

;; (defn bindE [k])

(defn mapE [f e]
  (event-stream [e] #(update-in % [:value] f)))

(def r (->> (receiverE) (mapE #(js/alert %))))

(defn doit []
  (send-event r "o")
  (send-event r "m")
  (send-event r "g"))