(ns alan
  (:require [tailrecursion.javelin   :as w]
            [tailrecursion.javelin.e :as e]
            [tailrecursion.javelin.dom :as dom])
  (:require-macros [tailrecursion.javelin.macros :refer [with with-let]]))

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
