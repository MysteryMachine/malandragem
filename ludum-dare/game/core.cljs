(ns game.core
  (:require [malandragem.core :as rl :refer [px]]))

(enable-console-print!)

(def color
  {:darkness "#080403"})

(defn default-tile [game [x y]]
  (rl/colored-tile (rem (js/Math.floor (/ (+ x y) 10)) 10)
                   (rem (js/Math.floor (/ (+ x y) 10)) 10)
                   (rem (js/Math.floor (/ (+ x y) 10)) 10)))

(defn draw-player [game [x y]]
  (rl/colored-tile 5 2 1))

(def levels
  {0 {:dimensions [100 100]
      :entities
      {:character
       {[2 3] draw-player}}
      :tiles {:default default-tile}}})

(def state
  (rl/data
   :levels levels
   ::state {::menu :init ::index 0}
   :state {:level 0 :viewport [0 0]}
   :entity-heiarchy [:item :character]
   :settings {:tile-dimensions [30 30]
              :screen-dimensions [0.8 0.8]}))

(def default-text
  {:color "white"
   :font-family "Consolas"
   :user-select "none"})

(defn menu-style [x y r d]
  {:style (merge
           default-text
           {:background-color "black"
             :height (px (+ y d))
             :width (px r)
             :position "absolute"
             :left (px x)})})

(defn text-box-style [x y r d]
  {:style (merge
           default-text
           {:background-color (:darkness color)
             :position "absolute"
             :top (px y)
             :height (px d)
             :width (px x)})})

(def game-box-style {:style {:position "absolute"}})

(defmulti render-menu ::menu :default :default)
(defmethod render-menu :default
  [state]
  [:div "menu"])

(defn render-textbox [menu]
  (map
   (fn [text]
     [:pre (str ">")])))

(def init-style
  (merge
   default-text
   {:position "fixed"
    :width "100%"
    :height "100%"
    :text-align "center"
    :background-color (:darkness color)}))

(def inner-init-style
  {:position "fixed"
   :margin "20% 10%"
   :width "80%"
   :height "60%"
   :font-size "1.5em"
   :font-weight "light"})

(def click-anywhere-style
  {:position "fixed"
   :bottom "20%"
   :width "100%"})

(def init-dialogue
  [[:div [:h1 "ASHZAR"]]
   [:div
    [:p
     "What a horrible tragedy has befallen the kingdom. "
     "The young heir, struck down before their time."]
    [:p
     "While the noble monarchs grieve, the heir's "
     "lover plots a scheme of revival."]]
   [:div
    [:p
     "Deep, in the ancient catacombs of the kingdom, lies the "
     "resting place of an ancient, forgotten civilization. "]
    [:p "Ashzar. The Kingdom of Light."]]
   [:div
    [:p
     "Before being mysteriously cast into darkness by a mysterious "
     "event, the kingdom was rumored to contain the secrets of bringing "
     "back the dead from eternal slumber."]]
   [:div
    [:p
     "Quietly, the heir's love opens the door, clutching their love "
     "in their arms. The heir dares not dally, knowing the monarchs "
     "disbelief in the ancient legends prevents them from seeing the "
     "import of the task at hand."]]
   [:div
    [:p
     "The lover grabs a bit of scrap, and feeds it to the light bending machine."]
    [:p
     "A terrible weapon, for a terrible task. The heir must be brought."]]
   [:div
    [:p
     "The human cost is a terrible tragedy."]]])

(count init-dialogue)

(defn handle-init-clicks [game]
  (fn []
   (let [index (-> game ::state ::index)]
      (println (::state game))
      (if (< (inc index) (count init-dialogue))
        (swap! state #(update-in % [::state ::index] inc))
        (swap! state #(-> %
                          (assoc-in [::state ::index] 0)
                          (assoc-in [::state ::menu] :game)))))))

(defn render-init [game]
  [:div {:style init-style
         :on-click (handle-init-clicks game)}
   [:div {:style inner-init-style}
    [:style "p{ padding-bottom: 2em; }"]
    (get init-dialogue (-> game ::state ::index))]
   [:div {:style click-anywhere-style}
    "-- CLICK ANYWHERE TO CONTINUE --"]])

(defn render-game [game]
  (let [[x y] (rl/game-dims game)
        [r d] (rl/remaining-space game)]
   [:div
     [:div game-box-style (rl/render game)]
     [:div (menu-style x y r d) (render-menu game)]
     [:div (text-box-style x y r d) (render-textbox game)]]))

(defn app []
  (let [game @state]
    (case (-> game ::state ::menu)
      :init (render-init game)
      :game (render-game game))))

(def root (. js/document (getElementById "app")))

(defn tick-game [game]
  game)

(rl/wander app state root {:time {:fn tick-game :fps 2}})

(defn on-js-reload [])
