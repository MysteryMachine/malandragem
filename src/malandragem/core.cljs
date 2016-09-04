(ns malandragem.core
  (:require [malandragem.impl :as impl]
            [reagent.core :as reagent]
            [clojure.core.async :refer [poll! chan]]))

(defn data [& {:as data}]
  (let [a (reagent/atom (merge impl/default-data data))]
    ((impl/resize-event a))
    a))

(defn wander [game-fn game-atom body]
  (let [time (-> @game-atom :settings :time)]
    (impl/clear-state!)
    (impl/register-state! game-atom body)
    (when time
      (let [keypresses (chan)
            keypress-fn (impl/register-keypresses keypresses)
            _ (js/window.addEventListener "keydown" keypress-fn)
            event (js/window.setInterval
                   (fn []
                     (let [old-state @game-atom
                           new-state ((:fn time) old-state keypresses)]
                       (reset! game-atom new-state)))
                   (/ 1000 (:fps time)))]
        (swap! impl/-state #(assoc % "keydown" keypress-fn))
        (swap! impl/-state #(assoc % :interval event))))
    (reagent/render-component [game-fn] body)))

(def get-level impl/get-level)

(defn render [game]
  (let [settings (:settings game)
        level (impl/get-level game)
        [dx dy] (:tile-dimensions settings)
        [xi yi] (-> game :state :viewport)]
    (->
     (impl/build-default game)
     (into
      (map (partial impl/draw-floor game))
      (impl/get-coords xi yi dx dy))
     (into (impl/draw-entities game level)))))

(defn game-dims [state] (-> state :rl/impl :rl/box-size))

(defn remaining-space [state] (-> state :rl/impl :rl/remaining-space))

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

(defn +tile [tile [x y] level] (assoc-in level [:tiles [x y]] tile))
(defn -tile [[x y] level] (update level :tiles #(dissoc % [x y] level)))

(defn +entity [tag [x y] entity level]
  (assoc-in level [:entities tag [x y]] entity))
(defn -entity [tag [x y] level]
  (update-in level [:entities tag] #(dissoc % [x y] level)))

(defn take-all! [keypress-chan]
  (loop [events []]
    (if-let [e (poll! keypress-chan)]
      (recur (conj events e))
      events)))

(defn mouse-pos [game] (-> game :state :mouse))

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

(defn level-path [game]
  [:levels (-> game :state :level)])

(defn entities-of-type [game etype]
  (when etype
    (-> game get-level :entities etype)))

(defn update-entity
  ([game [tag coord] new-coord new-entity &
    {:keys [solid-pass? player?]}]
   (let [level-path* (level-path game)
         path (conj level-path* :entities)
         entities (get-in game path)
         tiles (:tiles (get-in game level-path*))
         passed? (or solid-pass?
                     (not (impl/any-obstructions? entities tiles new-coord)))
         game* (if passed?
                 (move-entity game path entities tag coord new-coord new-entity)
                 game)]
     (if (and passed? player?)
       (update-viewport game* new-coord)
       game*))))

(defn near?
  ([[xi yi] [xf yf] i]
   (let [x* (- xi xf)
         y* (- yi yf)
         x (* x* x*)
         y (* y* y*)]
    (<= (+ x y) (* i i))))
  ([coord1 coord2]
   (near? coord1 coord2 1)))

(defn near-filter
  ([game coords filter-cond]
   (near-filter game coords 1 filter-cond))
  ([game [x y] i filter-cond]
   (let [level (get-level game)
         entities (:entities level)]
     (for [[_ v] entities
           x (range (- x i) (+ i x))
           y (range (- y i) (+ i y))
           :let [coord [x y]
                 e (get v coord)
                 ret [coord e]]
           :when (not (nil? e))
           :when (filter-cond coord e)]
       ret))))

(defn init-report [state]
  (fn [coord s]
   (fn []
     (swap!
      state
      #(->
        %
        (assoc-in [:state :report] s)
        (assoc-in [:state :mouse] coord))))))

(defn tick [game] (update game :time inc))
