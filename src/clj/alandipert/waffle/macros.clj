(ns alandipert.waffle.macros
  (:use [clojure.pprint :only [pprint]]))

;;; Mutation helper macros.

(defmacro with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~resource))

(defmacro with
  [resource & body]
  `(let [ret# ~resource] ~@body ret#))