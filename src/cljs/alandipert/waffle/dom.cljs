(ns alandipert.waffle.dom
  "Functions for extracting data from and manipulating the DOM."
  (:require [alandipert.waffle     :as w]
            [alandipert.waffle.e   :as e]
            [alandipert.waffle.e   :as b]
            [clojure.browser.event :as event]
            [goog.dom.forms        :as form])
  (:require-macros [alandipert.waffle.macros :refer [with-let]]))

(defn by-id
  "If id-or-elem is a string, returns the element with the specified
  id. Otherwise returns id-or-elem which is presumably an element."
  [id-or-elem]
  {:post [(identity %)]}
  (if (string? id-or-elem) (.getElementById js/document id-or-elem) id-or-elem))

(defn events
  [id-or-elem evts]
  {:pre [(or (string? evts)
             (every? string? evts))]}
  (w/changes
   (with-let [in (w/input (atom nil))]
     (event/listen (by-id id-or-elem)
                   (if (string? evts) evts (apply array evts))
                   #(reset! in %)))))

(defn value
  [id-or-elem]
  (let [elem (by-id id-or-elem)]
    (with-let [in (w/input (atom (form/getValue elem)))]
      (event/listen elem "input" #(reset! in (form/getValue elem))))))

(defn insert
  [cell id-or-elem fld & flds]
  )