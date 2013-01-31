(ns tailrecursion.javelin.macros
  (:use [clojure.pprint :only [pprint]]))

;;; Mutation helper macros.

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(defmacro with
  "Evaluates body and returns resource."
  [resource & body]
  `(with-let [res# ~resource] ~@body))
