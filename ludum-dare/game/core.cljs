(ns game.core
  (:require [malandragem.core :as rl :refer [px]]))

(enable-console-print!)

(def checkerboard
  {0 "white"
   1 "black"})

(defn default-tile [game [x y]]
  [:tile
   {:style
    {:background-color (checkerboard (rem (+ x y) 2))}}])

(defn draw-skull [game [x y]]
  [:skull
   {:style
    {:background-image "url(img/poison_skull.gif)"
     :background-repeat "no-repeat"
     :background-size "cover"}}])

(def levels
  {0 {:dimensions [100 100]
      :entities
      {:character
       {[2 3] draw-skull}}
      :tiles {:default default-tile}}})

(def state
  (rl/data
   :levels levels
   :state {:level 0}
   :entity-heiarchy [:item :character]
   :settings {:tile-dimensions [8 8]
              :screen-dimensions [0.8 0.9]}))

(defn game []
  (let [game @state
        [x y] (rl/game-dims game)
        [r d] (rl/remaining-space game)]
   [:div
     [:div {:style {:position "absolute"}} (rl/render game)]
     [:div {:style {:background-color "black"
                    :color "white"
                    :height (px (+ y d))
                    :width (px r)
                    :position "absolute"
                    :left (px x)}}
      [:div {:style {:text-align "center"}}
       [:h1 "VIDEO GAME"]
       [:button "Fight"]
       [:button "Smooch"]]]
    [:div {:style {:background-color "grey"
                   :color "white"
                   :position "absolute"
                   :top (px y)
                   :height (px d)
                   :width (px y)}}
     [:p "> The monster wants to fight!"]
     [:p "> But maybe smooch too, wow"]]]))

(rl/wander game state (. js/document (getElementById "app")))

(defn on-js-reload []) 
