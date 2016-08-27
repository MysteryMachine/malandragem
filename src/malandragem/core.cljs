(ns malandragem.core
  (:require [clojure.core.async :refer [chan <! >! poll!]]
            [reagent.core :as reagent])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defonce -state (atom {}))

(def default-container-style
  {:padding "0"
   :margin "0"
   :position "relative"})

(def default-data
  {:levels {}
   :state {:level ::none}})

(def default-tile-style
  {:background-color "#FFFFFF"
   :position "absolute"})

(defn get-level [game]
  (get (:levels game) (:level (:state game))))

(defn data [& kvs]
  (reagent/atom (into default-data (map vec) (partition 2 kvs))))

(defn transform-tile [default-style [t properties] & other-styles]
  (let [styles (into [default-style (:style properties)]
                     other-styles)]
    [:div (assoc properties :style (apply merge styles))]))

(defn tile-location-style [tile-size [x y] [vx vy]]
  {:left (str (* (- x vx) tile-size) "px")
   :top (str (* (- y vx) tile-size) "px")})

(defn draw-tile [game coord draw-fn]
  (let [tile-size-style (-> game ::impl ::tile-size-style)
        tile-size (-> game ::impl ::size)
        viewport (-> game :state :viewport)
        tile-loc-st (tile-location-style tile-size coord viewport)]
    (transform-tile default-tile-style
                    (draw-fn game coord)
                    tile-size-style
                    tile-loc-st)))

(defn draw-floor [game coord]
  (let [{:keys [tiles]} (get-level game)
        draw-fn (get tiles coord (:default tiles))]
    (draw-tile game coord draw-fn)))

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
    {::box-size [sx sy]
     ::remaining-space [right down]
     ::size tile-size*
     ::tile-size-style
     {:width tile-size :height tile-size
      :min-width tile-size :min-height tile-size
      :max-width tile-size :max-height tile-size}}))

(defn build-default [game]
  (let [[sx sy] (-> game ::impl ::box-size)
        box-size {:max-height sy :max-width sx
                  :height sy :width sx
                  :min-height sy :min-width sx}]
    [:div {:style (merge default-container-style box-size)}]))

(defn render [game]
  (let [settings (:settings game)
        level (get-level game)
        [dx dy] (:tile-dimensions settings)
        [xi yi] (-> game :state :viewport)]
    (->
     (build-default game)
     (into
      (map (partial draw-floor game))
      (get-coords xi yi dx dy))
     (into (draw-entities game level)))))

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
       (assoc game ::impl (resize-state (:settings game)))))))

(defn register-state! [game-atom body]
  (let [resize-fn (resize-event game-atom)]
    (resize-fn)
    (js/window.addEventListener "resize" resize-fn)
    (swap! -state #(assoc % "resize" resize-fn))))

(defn wander [game-fn game-atom body
              {:keys [time]}]
  (clear-state!)
  (register-state! game-atom body)
  (when time
    (let [event (js/window.setInterval (:fn time) (/ 1000 (:fps time)))]
      (swap! -state #(assoc % :interval event))))
  (reagent/render-component [game-fn] body))

(defn game-dims [state] (-> state ::impl ::box-size))

(defn remaining-space [state] (-> state ::impl ::remaining-space))

(defn px [s] (str s "px"))

(defn sprite [url]
  [:sprite
   {:style
    {:background-image (str "url(" url ")")
     :background-repeat "no-repeat"
     :background-size "cover"}}])

(defn colored-tile
  ([r g b]
   [:tile
    {:style
     {:background-color (str "#" r g b)}}])
  ([r1 r2 g1 g2 b1 b2]
   [:tile
    {:style
     {:background-color (str "#" r1 r2 g1 g2 b1 b2)}}]))
