(ns alandipert.waffle.macros
  (:use [clojure.pprint :only [pprint]]))

;;; Mutation helper macros.

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's cross between doto and with-open."
  [[binding resource] & body]
  `(let [res# ~binding] ~@body res#))

(defmacro with
  "Evaluates "
  [resource & body]
  `(let [res# ~resource] (do ~@body res#)))