(ns game.core
  (:require [malandragem.core :as rl]))

(enable-console-print!)

(def checkerboard
  {0 "white"
   1 "black"})

(defn default-tile [game [x y]]
  [:tile
   {:style
    {:background-color (checkerboard (rem (+ x y) 2))}}])

(def levels
  {0 {:dimensions [100 100]
      :entities {}
      :tiles {:default default-tile}}})

(def state
  (rl/data
   :levels levels
   :state {:level 0}
   :settings {:tile-dimensions [8 8]
              :screen-dimensions [1 1]}))

(defn game [] (rl/render @state))

(rl/wander game state (. js/document (getElementById "app")))

(defn on-js-reload [])
