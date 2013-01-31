(ns alan
  (:require [alandipert.waffle   :as w]
            [alandipert.waffle.e :as e]
            [alandipert.waffle.dom :as dom])
  (:require-macros [alandipert.waffle.macros :refer [with with-let]]))

;;; TODO dom tests

(let [in (w/input (atom nil))
      name (-> (w/changes in)
               ((w/lift #(.toUpperCase %)))
               ((w/lift #(str % (if (seq %) "?")))))]

  (dom/insert! name "name-out" "innerHTML")

  (doseq [s "bob"] (reset! in s))

  (.log js/console "span content" (.-innerHTML (dom/by-id "name-out")))
  (assert (= (.-innerHTML (dom/by-id "name-out")) "BOB?")))

(.log js/console "__exit__")
