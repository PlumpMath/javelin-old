(ns alandipert.waffle.b
  (:require
    [alandipert.waffle  :as w]
    [cljs.core          :as cljs])
  (:refer-clojure :exclude [map identity filter])
  (:require-macros [alandipert.waffle.macros :refer [maptemplate]]))


(defn no-repeats
  "Returns the value of the atom whenever it changes."
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
                (no-repeats to-lift)
                (fn [& atoms]
                  (apply w/lift to-lift atoms))))

(defn lift
  ([f]
            (fn [& atms]
              (no-repeats (apply (partial w/lift f) atms)))))

(maptemplate
  (fn [sym] `(def ~sym (~'lift ~(symbol (str 'cljs) (str sym)))))
  [map identity filter])