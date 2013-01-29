(ns alandipert.waffle.e
  (:require
    [alandipert.waffle  :as w]
    [cljs.core          :as cljs])
  (:refer-clojure :exclude [map identity filter]))

(def map      (w/lift cljs/map))
(def identity (w/lift cljs/identity))
(def filter   (w/lift cljs/filter))
