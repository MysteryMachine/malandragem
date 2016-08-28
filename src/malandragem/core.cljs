(ns malandragem.core
  (:require [clojure.core.async :refer [chan put! poll!]]
            [clojure.data]
            [reagent.core :as reagent]))

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

(defn data [& {:as data}]
  (reagent/atom (merge default-data data)))

(defn transform-tile [default-style [t properties] & other-styles]
  (let [styles (into [default-style (:style properties)]
                     other-styles)]
    [:div (assoc properties :style (apply merge styles))]))

(defn tile-location-style [tile-size [x y] [vx vy]]
  {:left (str (* (- x vx) tile-size) "px")
   :top (str (* (- y vy) tile-size) "px")})

(defn draw-tile [game coord drawer]
  (let [draw-fn (:draw drawer)
        tile-size-style (-> game ::impl ::tile-size-style)
        tile-size (-> game ::impl ::size)
        viewport (-> game :state :viewport)
        tile-loc-st (tile-location-style tile-size coord viewport)] 
    (transform-tile default-tile-style
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

(defn register-keypresses [keypress-chan]
  (fn [k]
    (put! keypress-chan (.-key k))))

(defn wander [game-fn game-atom body
              {:keys [time]}]
  (clear-state!)
  (register-state! game-atom body)
  (when time
    (let [keypresses (chan)
          keypress-fn (register-keypresses keypresses)
          _ (js/window.addEventListener "keydown" keypress-fn)
          event (js/window.setInterval
                 (fn []
                   (let [old-state @game-atom
                         new-state ((:fn time) old-state keypresses)]
                    (reset! game-atom new-state)))
                 (/ 1000 (:fps time)))]
      (swap! -state #(assoc % "keydown" keypress-fn))
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
  ([color]
   [:tile
    {:style
     {:background-color color}}]))

(defn with-props [tile & {:as props}]
  (update tile 1 #(merge % props)))

(defn blit-rect
  ([tile-fn xi yi dx dy level]
   (blit-rect tile-fn xi yi dx dy true level))
  ([tile-fn xi yi dx dy overwrite level]
   (let [tiles (:tiles level)]
     (update
      level :tiles
      (fn [t]
        (into t
              (for [x (range xi (+ xi dx))
                    y (range yi (+ yi dy))
                    :let [coord [x y]]
                    :when (or overwrite
                              (not (get tiles coord)))]
                [coord tile-fn])))))))

(defn blit-room
  ([outer-tile inner-tile wall-width [xi yi] [dx dy] overwrite level]
   (->>
    level
    (blit-rect outer-tile xi yi dx dy overwrite)
    (blit-rect inner-tile
               (+ wall-width xi)
               (+ wall-width yi)
               (- dx (* 2 wall-width))
               (- dy (* 2 wall-width)))))
  ([outer-tile inner-tile wall-width [xi yi] [dx dy] level]
   (blit-room outer-tile inner-tile wall-width [xi yi] [dx dy] false level)))

(defn +tile [tile x y level] (assoc-in level [:tiles [x y]] tile))
(defn -tile [x y level] (update level :tiles #(dissoc % [x y] level)))

(defn take-all! [keypress-chan]
  (loop [events []]
    (if-let [e (poll! keypress-chan)]
      (recur (conj events e))
      events)))

(defn any-obstructions? [entities tiles coord]
  (or
   (:solid (get tiles coord))
   (some
    (fn [[k v]] (:solid (get v coord)))
    tiles)))

(defn update-viewport [game [x y]]
  (let [[tx ty] (-> game :settings :tile-dimensions)
        dx (/ (dec tx) 2)
        dy (/ (dec ty) 2)
        newx (- x dx)
        newy (- y dy)]
    (assoc-in game [:state :viewport] [newx newy])))

(defn move-entity [game path entities tag coord new-coord new-entity]
  (let [updated-entities
        (update
         entities tag
         #(-> % (dissoc coord) (assoc new-coord new-entity)))]
   (assoc-in game path updated-entities)))

(defn update-entity
  ([game [tag coord] new-coord new-entity &
    {:keys [solid-pass? player?]}]
   (let [level-path [:levels (-> game :state :level)]
         path (conj level-path :entities)
         entities (get-in game path)
         tiles (:tiles (get-in game level-path))
         passed? (or solid-pass?
                     (not (any-obstructions? entities tiles new-coord)))
         game* (if passed?
                 (move-entity game path entities tag coord new-coord new-entity)
                 game)]
     (if (and passed? player?)
       (update-viewport game* new-coord)
       game*))))
