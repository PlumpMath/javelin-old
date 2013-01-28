(ns alandipert.waffle.macros)

(defmacro with-let
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))