(ns malandragem.impl
  "An implementation namespace for Malandragem.
   See malandragem.core for the public API."
  (:require [clojure.core.async :refer [put!]]
            [reagent.core :as reagent]))

(defn get-level [game]
  (get (:levels game) (:level (:state game))))

(defonce -state (atom {}))

(def default-container-style
  {:padding "0"
   :margin "0"
   :position "relative"})

(def default-data
  {:levels {}
   :state {:level :rl/none}
   :time 0})

(def default-tile-style
  {:background-color "#FFFFFF"
   :position "absolute"})

(defn transform-tile [default-style [t properties] & other-styles]
  (let [styles (into [default-style (:style properties)]
                     other-styles)]
    [:div (assoc properties :style (apply merge styles))]))

(defn tile-location-style [tile-size [x y] [vx vy]]
  {:left (str (* (- x vx) tile-size) "px")
   :top (str (* (- y vy) tile-size) "px")})

(defn draw-tile [game coord drawer]
  (let [draw-fn (:draw drawer)
        tile-size-style (-> game :rl/impl :rl/tile-size-style)
        tile-size (-> game :rl/impl :rl/size)
        viewport (-> game :state :viewport)
        tile-loc-st (tile-location-style tile-size coord viewport)]
    (transform-tile
     default-tile-style
     (draw-fn game coord)
     tile-size-style
     tile-loc-st)))

(defn draw-floor [game coord]
  (let [{:keys [tiles]} (get-level game)
        drawer (get tiles coord (:default tiles))]
    (draw-tile game coord drawer)))

(defn draw-entities [game level]
  (let [heiarchy (:entity-heiarchy game)
        level-entities (:entities level)
        entities (into [] (mapcat (partial get level-entities)) heiarchy)]
    (into [:div] (map #(draw-tile game (first %) (second %))) entities)))

(defn get-coords [xi yi dx dy]
  (for [x (range xi (+ xi dx))
        y (range yi (+ yi dy))]
    [x y]))

(defn resize-state [settings]
  (let [xscrn window.innerWidth
        yscrn window.innerHeight
        [sx sy] (:screen-dimensions settings)
        [xdims ydims] (:tile-dimensions settings)
        xsize (js/Math.floor (/ (* sx xscrn) xdims))
        ysize (js/Math.floor (/ (* sy yscrn) ydims))
        tile-size* (if (< xsize ysize) xsize ysize)
        tile-size (str tile-size* "px")
        sx (* tile-size* xdims)
        sy (* tile-size* ydims)
        right (- xscrn sx)
        down (- yscrn sy)]
    {:rl/box-size [sx sy]
     :rl/remaining-space [right down]
     :rl/size tile-size*
     :rl/tile-size-style
     {:width tile-size :height tile-size
      :min-width tile-size :min-height tile-size
      :max-width tile-size :max-height tile-size}}))

(defn build-default [game]
  (let [[sx sy] (-> game :rl/impl :rl/box-size)
        box-size {:max-height sy :max-width sx
                  :height sy :width sx
                  :min-height sy :min-width sx}]
    [:div {:style (merge default-container-style box-size)}]))

(defn clear-state! []
  (doseq [[k v] @-state]
    (cond
      (= k :interval) (js/window.clearInterval v)
      :default (js/window.removeEventListener k v))))

(defn resize-event [game-atom]
  (fn []
    (swap!
     game-atom
     (fn [game]
       (assoc game :rl/impl (resize-state (:settings game)))))))

(defn register-state! [game-atom body]
  (let [resize-fn (resize-event game-atom)]
    (js/window.addEventListener "resize" resize-fn)
    (swap! -state #(assoc % "resize" resize-fn))))

(defn register-keypresses [keypress-chan]
  (fn [k]
    (put! keypress-chan (.-key k))))

(defn any-obstructions? [entities tiles coord]
  (or
   (true? (:solid (get tiles coord)))
   (some
    (fn [[k v]] (true? (:solid (get v coord))))
    tiles)))
