(ns alandipert.waffle.b
  (:require
    [alandipert.waffle  :as w]
    [cljs.core          :as cljs])
  (:refer-clojure :exclude [map identity filter])
  (:require-macros [alandipert.waffle.macros :refer [maptemplate]]))

(defn changes
  [cell]
  )

(defn lift
  [f]
  )

(maptemplate
  (fn [sym] `(def ~sym (~'lift ~(symbol (str 'cljs) (str sym)))))
  [map identity filter])
