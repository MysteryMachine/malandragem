(ns malandragem.core
  (:require [clojure.core.async :refer [chan <! >! poll!]]
            [reagent.core :as reagent])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def default-container
  {:style
   {:padding "0"
    :margin "0"}})

(def default-data
  {:levels {}
   :state {:level ::none}})

(def default-tile-style
  {:background-color "#FFFFFF"
   :position "fixed"})

(def default-tile-size-style
  {:padding "0"
   :margin "0"
   :display "block"})

(defn get-level [game]
  (get (:levels game) (:level (:state game))))

(defn data [& kvs]
  (reagent/atom
   (into default-data (map vec) (partition 2 kvs))))

(defn transform-tile [default-style [t properties] & other-styles]
  (let [styles (into [default-style (:style properties)]
                     other-styles)]
    [:div (assoc properties :style (apply merge styles))]))

(defn tile-location-style [tile-size [x y]]
  {:left (str (* x tile-size) "px")
   :top (str (* y tile-size) "px")})

(defn draw-tile [game coord]
  (let [{:keys [tiles]} (get-level game)
        tile-size-style (-> game ::impl ::tile-size-style)
        tile-size ( ::size tile-size-style)
        draw-fn (get tiles coord (:default tiles))
        tile-location (tile-location-style tile-size coord)]
    (transform-tile default-tile-style
                    (draw-fn game coord)
                    tile-size-style
                    tile-location)))

(defn draw-entity [game level entity])

(defn get-coords [x y]
  (for [x (range x)
        y (range y)]
    [x y]))

(defn get-tile-size-style [settings]
  (let [xscrn window.innerWidth
        yscrn window.innerHeight
        [sx sy] (:screen-dimensions settings)
        [xdims ydims] (:tile-dimensions settings)
        xsize (js/Math.floor (/ (* sx xscrn) xdims))
        ysize (js/Math.floor (/ (* sy yscrn) ydims))
        tile-size* (if (< xsize ysize) xsize ysize)
        tile-size (str tile-size* "px")]
    (merge default-tile-size-style
           {:width tile-size :height tile-size
            :min-width tile-size :min-height tile-size
            :max-width tile-size :max-height tile-size
            ::size tile-size*})))

(defn build-default [game])

(defn render [game]
  (let [settings (:settings game)
        level (get-level game)
        [x y] (:tile-dimensions settings)]
    (->
     (build-default game)
     (into
      (map (partial draw-tile game))
      (get-coords x y))
     (into
      (map (partial draw-entity game level))
      (:entities level)))))

(defn resize-event [game-atom]
  (fn []
    (swap!
     game-atom
     (fn [game]
       (assoc-in game [::impl ::tile-size-style]
                 (get-tile-size-style (:settings game)))))))

(defn register-state! [game-atom body]
  (let [resize-fn (resize-event game-atom)]
    (resize-fn)
    (js/window.addEventListener "resize" resize-fn)))

(defn wander [game-fn game-atom body]
  (register-state! game-atom body)
  (reagent/render-component [game-fn] body))

