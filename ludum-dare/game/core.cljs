(ns game.core
  (:require [malandragem.core :as rl :refer [px]]))

(enable-console-print!)
(declare state)

(defn report [s]
  (fn [] (swap! state #(assoc-in % [:state :report] s))))

(def color
  {:darkness "#080403"})

(def default-tile
  {:draw
   (fn [game [x y]]
     (rl/colored-tile (:darkness color)))
   :solid true})

(def player
  {:draw
   (fn [game [x y]]
     (rl/with-props
       (rl/colored-tile "#271")
       :on-mouse-over (report "The starcrossed lover. You!")))
   :solid true})

(def steel-wall
  {:draw
   (fn [game [x y]]
     (rl/with-props
       (rl/colored-tile "#222")
       :on-mouse-over (report "An ancient, steel wall.")))
   :solid true})

(def steel-floor
  {:draw
   (fn [game [x y]]
     (rl/with-props
       (rl/colored-tile "#393939")
       :on-mouse-over (report "An ancient, steel floor.")))
   :solid false})

(def water
  {:draw
   (fn [game [x y]]
     (rl/with-props
       (rl/colored-tile "#226")
       :on-mouse-over (report "Murky water.")))
   :solid true})

(def light-machine
  {:draw
   (fn [game [x y]]
     (rl/with-props
       (rl/colored-tile "#551")
       :on-mouse-over (report "The light bending machine of old.")))
   :solid true})

(def console
  {:draw
   (fn [game [x y]]
     (rl/with-props
       (rl/colored-tile "#383235")
       :on-mouse-over (report "The light bending machine of old.")))
   :solid true})

(defn => [& s]
  (let [[style s*] (if (map? (first s))

                     [(first s) (rest s)]
                     [{} s])]
    [:div {:style {:display "flex" :padding-bottom "1em"}}
     [:pre {:style {:margin "0"}} ">  "]
     [:div {:style (merge {:float "left" :display "inline"} style)} (apply str s)]]))

(defn room [x y dx dy level]
  (rl/blit-room steel-wall steel-floor 1 [x y] [dx dy] level))

(defn entrance-hallway [level]
  (->>
   level
   (room 48 50 9 26)
   (rl/blit-rect default-tile 58 75 9 2)))

(defn main-chambers [level]
  (->>
   level
   (room 40 25 25 27)
   (room 23 32 16 13)
   (room 28 43 6 5)
   (rl/blit-rect water 26 35 10 7)))

(defn hallways [level]
  (->>
   level
   (room 20 46 40 5)
   (room 20 26 40 5)
   (room 12 15 10 50)
   (room 0 25 15 28)))

(defn objects [level]
  (->>
   level
   (rl/blit-room console light-machine 1 [14 17] [6 3] true)))

(defn construct-level []
  (->>
   {:entities
    {:character
     {[17 22] player}}
    :tiles {:default default-tile}}
   (entrance-hallway)
   (main-chambers)
   (hallways)
   (objects)))

(def levels {0 (construct-level)})

(def state
  (rl/data
   :levels levels
   ::state {::menu :game ::index 0}
   :state {:level 0 :viewport [0 14] :debug true}
   :text [(=> "The heavy iron door locks in once opened, leaving the facility "
              "exposed to the elements. You hear marching from far away. "
              "You must make haste, and prepare your defenses. Find ore "
              "and feed it to the light machine.")]
   :entity-heiarchy [:item :character]
   :settings {:tile-dimensions [35 17]
              :screen-dimensions [0.85 0.9]}))

(def default-text
  {:color "white"
   :font-family "Consolas"
   :user-select "none"})

(defn menu-style [x y r d]
  (merge
   default-text
   {:background-color (:darkness color)
    :height (px (+ y d))
    :width (px r)
    :top (px 0)
    :overflow "hiden"
    :position "absolute"
    :left (px x)}))

(def inner-menu-style {:padding "20px"})

(defn text-box-style [x y r d]
  (merge
   default-text
   {:background-color (:darkness color)
    :position "absolute"
    :overflow "hidden"
    :top (px y)
    :left (px 0)
    :height (px d)
    :width (px x)}))

(def game-box-style {:style {:position "absolute"}})

(defn inner-menu [& forms]
  (into [:div {:style inner-menu-style}] forms))

(defmulti render-menu ::menu :default :default)
(defmethod render-menu :default
  [game]
  (inner-menu
   (-> game :state :report)))

(defn render-textbox [game]
  (into [:div
         {:style
          {:height "100%"
           :width "100%"
           :overflow-y "scroll"
           :padding "20px"}}]
        (:text game)))

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

(defn handle-init-clicks [game]
  (fn []
   (let [index (-> game ::state ::index)]
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
    [:div {:style game-box-style} (rl/render game)]
    [:div {:style (menu-style x y r d)} (render-menu game)]
    [:div {:style (text-box-style x y r d)} (render-textbox game)]]))

(defn app []
  (let [game @state]
    (case (-> game ::state ::menu)
      :init (render-init game)
      :game (render-game game))))

(def root (. js/document (getElementById "app")))

(defn get-player [game]
  (-> (rl/get-level game) :entities :character first))

(defn move-player [game dx dy]
  (let [[[x y] character] (get-player game)]
    (rl/update-entity
     game
     [:character [x y]]
     [(+ x dx) (+ y dy)]
     character
     :player? true)))

(defmulti handle-input (fn [game event] event) :default :default)
(defmethod handle-input :default [game event] game)
(defmethod handle-input "ArrowLeft"
  [game event]
  (move-player game -1 0))
(defmethod handle-input "ArrowRight"
  [game event]
  (move-player game 1 0))
(defmethod handle-input "ArrowDown"
  [game event]
  (move-player game 0 1))
(defmethod handle-input "ArrowUp"
  [game event]
  (move-player game 0 -1))

(defn handle-inputs [game events]
  (loop [game game
         events events]
    (if (seq events)
      (recur (handle-input game (first events)) (rest events))
      game)))

(defn tick [game]
  game)

(defn tick-game! [game keypresses]
  (let [events (set (rl/take-all! keypresses))]
    (->
     game
     (handle-inputs events)
     (tick))))

(rl/wander app state root {:time {:fn tick-game! :fps 5}})

(defn on-js-reload [])
