(ns alandipert.waffle.b
  (:require
    [alandipert.waffle  :as w]
    [cljs.core          :as cljs])
  (:refer-clojure :exclude [map identity filter]))

(defn no-repeats
  "Stops propagation of repeated values from this FRP atom."
  [atm]
  (doto atm
    (w/wrap-update-fn!
      (fn [oldfn]
        (let [oldval @atm
              retval (oldfn)
              newval @atm]
          (or (and (not= :w/halt retval)
                   (not= oldval newval))
              :w/halt))))))

(def lift     (comp no-repeats w/lift))

(def map      (lift cljs/map))
(def identity (lift cljs/identity))
(def filter   (lift cljs/filter))
