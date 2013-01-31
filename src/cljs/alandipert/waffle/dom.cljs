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
  "Cell of evts type events fired by id-or-elem.
  evts may be a string or an array of strings.
  Strings should be lower case.

  For a full list of available event type strings, see
  http://closure-library.googlecode.com/svn/docs/closure_goog_events_eventtype.js.source.html"
  [id-or-elem evts] {:pre [(or (string? evts) (every? string? evts))]}
  (w/changes
   (with-let [in (w/input (atom nil))]
     (event/listen (by-id id-or-elem)
                   (if (string? evts) evts (apply array evts))
                   #(reset! in %)))))

(defn value
  "Cell of the form input id-or-elem."
  [id-or-elem]
  (let [elem (by-id id-or-elem)]
    (with-let [in (w/input (atom (form/getValue elem)))]
      (event/listen elem "input" #(reset! in (form/getValue elem))))))

(defn aset-in
  "Sets the value in a nested JavaScript array, where ks is a sequence
  of fields."
  [arr ks value]
  (aset (reduce #(aget %1 %2) arr (butlast ks)) (last ks) value))

(defn insert!
  "Inserts the value of cell into id-or-elem.  If no fields are
supplied, inserts into the element's 'value' field.  If more than one
fld is supplied, sets the nested field."
  ([cell id-or-elem]
     (insert! id-or-elem "value"))
  ([cell id-or-elem fld & flds]
     (let [elem (by-id id-or-elem)]
       ((w/lift #(aset-in elem (list* fld flds) %)) cell))))