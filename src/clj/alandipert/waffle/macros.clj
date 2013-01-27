(ns alandipert.waffle.macros
  "Syntax macros for working FRP functions.  Alias to - for maximum optimo."
  (:refer-clojure :exclude [>]))

(defmacro >
  "xE threading macro.  When this namespace is aliased as -, you can
  do things like:

  (-/> mapE e1 .toUpperCase js/alert) which is equivalent to

  (->> e1 (mapE #(.toUpperCase %)) (mapE #(js/alert %)))"
  ([f e form] (if (seq? form)
                (with-meta
                  `(~f (fn [v#] (~(first form) ~@(next form) v#)) ~e)
                  (meta form))
                `(~f (fn [v#] (~form v#)) ~e)))
  ([f e form1 form2 & more] `(> ~f (> ~f ~e ~form1) ~form2 ~@more)))