(ns alan
  (:require [alandipert.waffle   :as w]
            [alandipert.waffle.e :as e]
            [alandipert.waffle.dom :as dom])
  (:require-macros [alandipert.waffle.macros :refer [with with-let]]))

(set! cljs.core/*print-meta* true)

(def log #(js/console.log %))

(defn pr-cell
  [cell]
  (log {:v @cell
        :r (-> cell meta :alandipert.waffle/rank)
        :s (-> cell meta :alandipert.waffle/sinks)
        :f (-> cell meta :alandipert.waffle/thunk)}))

(defn pr** [& cells] (mapv pr-cell cells))

(def identity*  (w/lift identity))
(def inc*       (w/lift inc))
(def pr*        (w/lift log))
(def odd?*      (w/lift odd?))
(def args       #(vec %&))


;;; DOM: inserting and extracting
;;; Thu Jan 31 07:23:28 EST 2013

(defn ^:export doit []

  (defn random-color []
    (str "#" (.toString (rand-int 16777216) 16)))

  (let [keypresses (w/changes (dom/events (.-body js/document) "keypress"))
        name (-> (dom/value "name")
                 ((w/lift #(.toUpperCase %)))
                 ((w/lift #(str % (if (seq %) "?")))))]
    (dom/insert! name "yourname" "innerHTML")
    (dom/insert! ((w/lift random-color) keypresses)
                 js/document "body" "style" "backgroundColor")

    (pr* keypresses)

    ))


;; ;;;
;; ;;; LazySeq experiments, Thu Jan 31 04:16:09 EST 2013
;; ;;;

;; (extend-type cljs.core/Atom
;;   cljs.core/ISeqable
;;   (-seq [atm] atm)
;;   cljs.core/ISeq
;;   ;; If we deref @atm here in -first, we'd lose the connection to the
;;   ;; underlying behavior.  Thus, all function arguments to
;;   ;; LazySeq-based cells must be lifted.  The necessity of lifting
;;   ;; arguments is what kills LazySeq-based cells.
;;   (-first [atm] atm)
;;   (-rest [atm] atm))

;; (defn ->cell
;;   [seq]
;;   (let [thunk (.-x seq)
;;         [cell] (thunk)] ;(thunk) returns a cljs.core/Cons of [v, thunk]
;;     (with-let [seq (atom @cell)]
;;       ;since -first is a cell we deref here and push the value to seq
;;       (w/make-input-cell seq #(reset! seq @(first (thunk))))
;;       (w/attach! [cell] seq))))

;; (defn doit []

;;   ;; ->cell with cljs.core/map. note required use of inc*
;;   ;; (let [n (w/input (atom 0))
;;   ;;       inc* (w/lift inc)
;;   ;;       r (->cell (map inc* n))]
;;   ;;   (pr* r)
;;   ;;   (.setInterval js/window #(swap! n inc) 1000))


;;   ;; (let [c (w/input (atom "a"))
;;   ;;       next-char #(.fromCharCode js/String (inc (.charCodeAt %)))
;;   ;;       sep (identity* ",")
;;   ;;       chrs (->cell (interpose sep c))]

;;   ;;   (pr* chrs)

;;   ;;   (.setInterval js/window #(swap! c next-char) 1000))

;;   (let [c (w/input (atom "a"))
;;         chrs ((w/lift interpose) "," c)
;;         next-char #(.fromCharCode js/String (inc (.charCodeAt %)))]

;;     (pr* chrs)

;;     (.setInterval js/window #(swap! c next-char) 1000))
;;   )
