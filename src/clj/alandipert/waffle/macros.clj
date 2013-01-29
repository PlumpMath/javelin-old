(ns alandipert.waffle.macros
  (:use [clojure.pprint :only [pprint]]))

(defmacro with-let
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(defmacro maptemplate
  [template-fn coll]
  `(do ~@(map `~#((eval template-fn) %) coll)))