(ns malandragem.core
  (:require [clojure.core.async :refer [chan <! >! poll!]]
            [reagent.core :as reagent])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

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
  (reagent/atom
   (into default-data (map vec) (partition 2 kvs))))

(defn transform-tile [default-style [t properties] & other-styles]
  (let [styles (into [default-style (:style properties)]
                     other-styles)]
    (println [:div (assoc properties :style (apply merge styles))])
    [:div (assoc properties :style (apply merge styles))]))

(defn tile-location-style [tile-size [x y]]
  {:left (str (* x tile-size) "px")
   :top (str (* y tile-size) "px")})

(defn draw-tile [game coord]
  (let [{:keys [tiles]} (get-level game)
        tile-size-style (-> game ::impl ::tile-size-style)
        tile-size (-> game ::impl ::size)
        draw-fn (get tiles coord (:default tiles))
        tile-location (tile-location-style tile-size coord)]
    (transform-tile default-tile-style
                    (draw-fn game coord)
                    tile-size-style
                    tile-location)))

(defn draw-entity [game level entity]
  )

(defn draw-entities [game level]
  [:div])

(defn get-coords [x y]
  (for [x (range x)
        y (range y)]
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
        [x y] (:tile-dimensions settings)]
    (->
     (build-default game)
     (into
      (map (partial draw-tile game))
      (get-coords x y))
     (into (draw-entities game level)))))

(defn resize-event [game-atom]
  (fn []
    (swap!
     game-atom
     (fn [game]
       (assoc game ::impl (resize-state (:settings game)))))))

(defn register-state! [game-atom body]
  (let [resize-fn (resize-event game-atom)]
    (resize-fn)
    (js/window.addEventListener "resize" resize-fn)))

(defn wander [game-fn game-atom body]
  (register-state! game-atom body)
  (reagent/render-component [game-fn] body))

(defn game-dims [state] (-> state ::impl ::box-size))

(defn remaining-space [state] (-> state ::impl ::remaining-space))

(defn px [s] (str s "px"))
