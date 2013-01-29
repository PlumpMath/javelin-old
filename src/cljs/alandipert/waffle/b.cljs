(ns alandipert.waffle.b
  (:require
    [alandipert.waffle  :as w]
    [cljs.core          :as cljs])
  (:refer-clojure :exclude [map identity filter])
  (:require-macros [alandipert.waffle.macros :refer [maptemplate]]))

(defn no-repeats
  "Stops propagation of repeated values from this FRP atom."
  [atm]
  (doto atm
    (w/wrap-thunk!
      (fn [oldfn]
        (let [oldval @atm
              retval (oldfn)
              newval @atm]
          (or (and (not= :w/halt retval)
                   (not= oldval newval))
              :w/halt))))))

(def lift     (fn [to-lift]
                (fn [& atoms]
                  (apply w/lift (no-repeats to-lift) atoms))))

(maptemplate
  (fn [sym] `(def ~sym (~'lift ~(symbol (str 'cljs) (str sym)))))
  [map identity filter])