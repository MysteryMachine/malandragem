(ns game.core
  (:require [malandragem.core :as rl :refer [px]]))

(enable-console-print!)
(declare state machines report)

(def color
  {:darkness "#080403"
   :silver "#313235"
   :red "#522"
   :green "#252"})

(def no-purchase-text "Can't place purchase here")

(defn purchasing? [game] (= :purchase(-> game ::state ::menu)))

(defn cancel-purchase [game]
  (assoc-in game [::state ::menu] nil))

(defn complete-purchase []
  (swap!
   state
   (fn [game]
     (when (not= (-> game :state :report) no-purchase-text)
       (let [[machine ore water coal] (-> game ::state ::purchase)
             ore* (-> game :resources :ore)
             water* (-> game :resources :water)
             coal* (-> game :resources :coal)]
        (->
          game
          (update-in
           (rl/level-path game)
           (partial rl/+entity
                    :machine
                    (get machines machine)
                    (rl/mouse-pos game)))
          (assoc
           :resources
           {:ore  (- ore* ore)
            :water (- water* water)
            :coal (- coal* coal)})
          cancel-purchase))))))

(defn cancel-purchase! []
  (cancel-purchase @state))

(defn game-tile
  [desc tcolor & {:keys [solid] :as props}]
  (assoc
   props :draw
   (let [hover-color (if solid (:red color) (:green color))
         place-desc (if solid
                      no-purchase-text
                      "Click to place your")]
     (fn [game [x y :as coords]]
       (let [hover? (= coords (rl/mouse-pos game))
             p? (purchasing? game)
             p-hov? (and hover? p?)]
         (rl/with-props
           (rl/colored-tile (if p-hov? hover-color tcolor))
           :on-click (when p? complete-purchase)
           :on-mouse-over
           #((report
              coords
              (if (purchasing? @state) place-desc desc)))))))))

(def default-tile
  (game-tile ""
   (:darkness color)
   :solid true
   :name :default))

(def player
  (game-tile
   "The starcrossed lover. You!"
   "#271"
   :solid true
   :name :player))

(def steel-wall
  (game-tile
   "An ancient, steel wall."
   "#222"
   :solid true
   :name :steel-wall))

(def steel-floor
  (game-tile
   "An ancient, steel floor."
   "#393939"
   :solid false
   :name :steel-floor))

(def water
  (game-tile
   "Murky water."
   "#226"
   :solid true
   :name :water))

(def light-machine
  (game-tile
   "The light bending machine of old."
   "#551"
   :solid true
   :name :light-machine))

(def console
  (game-tile
   "The light bending machine of old."
   "#383235"
   :solid true
   :name :console))

(def coal-machine
  (game-tile
   "A machine that converts carbon to coal."
   "#745"
   :solid false
   :name :coal-machine))

(def drill-machine
  (game-tile
   "A machine that drills deeply with lasers."
   "#888"
   :solid false
   :name :drill-machine))

(def dehydrator
  (game-tile
   "Sucks out oxygen from the air."
   "#494"
   :solid false
   :name :dehydrator))

(def turret
  (game-tile
   "A machine used for killing."
   "#F00"
   :solid false
   :name :turret))

(def human-trans-machine
  (game-tile
   "You've done it now."
   "#000"
   :solid false
   :name :human-trans-machine))

(def machines
  {"Coalescer" coal-machine
   "Drill" drill-machine
   "Dehydrator" dehydrator
   "Light Turret" turret
   "Human Transmutation Machine" human-trans-machine})

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
     {[17 22] player}
     :machine {}}
    :tiles {:default default-tile}}
   (entrance-hallway)
   (main-chambers)
   (hallways)
   (objects)))

(def levels {0 (construct-level)})

(def state
  (rl/data
   :levels levels
   ::state {::mode :game ::index 0 ::menu :default}
   :state {:level 0 :viewport [0 14] :debug true}
   :text [(=> "The heavy iron door locks in once opened, leaving the facility "
              "exposed to the elements. You hear marching from far away. "
              "You must make haste, and prepare your defenses. Find ore "
              "and feed it to the light machine.")]
   :entity-heiarchy [::machine :character]
   :resources {:water 10 :coal 10 :ore 10}
   :settings {:tile-dimensions [35 17]
              :screen-dimensions [0.85 0.9]}))

(def report (rl/init-report state))

(def default-text
  {:color "white"
   :font-family "Consolas"
   :user-select "none"
   :line-height 1})

(defn menu-style [x y r d]
  (merge
   default-text
   {:background-color (:darkness color)
    :height (px (+ y d))
    :width (px r)
    :top (px 0)
    :overflow "hidden"
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

(defn purchase [& [name ore water coal :as args]]
  (swap! state
         #(->
           %
           (assoc-in [::state ::menu] :purchase)
           (assoc-in [::state ::purchase] args)
           (assoc-in [:state :report] nil))))

(defn build-button [game name desc [ore water coal]]
  (let [ore* (-> game :resources :ore)
        water* (-> game :resources :water)
        coal* (-> game :resources :coal)
        enough? (and (> ore* ore)
                     (> water* water)
                     (> coal* coal))]
    [:div {:style {:border-style "solid"
                   :border-width "1px"
                   :border-color (if enough? "white" "grey")
                   :color (if enough? "white" "grey")
                   :padding "10px"
                   :margin-bottom "10px"
                   :cursor (when enough? "pointer")}
           :on-click (when enough? #(purchase name ore water coal))
           :on-mouse-over
           (report
            [-1 -1]
            [:div
             [:div {:style {:padding-bottom "10px"}}
              desc]
             [:div {:style {:text-decoration "underline"
                            :padding-bottom "10px"}}
              "Cost:"]
             [:pre "Ore   :  " ore]
             [:pre "Water :  " water]
             [:pre "Coal  :  " coal]])}
     [:div {:style {:cursor (when enough? "pointer")}} name]]))

(defn report-menu [game right-space]
  [:div {:style {:position "fixed" :bottom "20px"
                 :width (px (- right-space 40))
                 :height "140px"
                 :border-style "solid"
                 :border-width "1px"
                 :border-color "white"}}
   [:div {:style {:padding "10px"}}
    (-> game :state :report)]])

(defn resource-panel [game]
  [:div
   [:div {:style {:text-decoration "underline"
                  :padding-bottom "20px"
                  :line-height "1"}}
    "Resources"]
   [:div {:style {:margin-bottom "20px"
                  :padding "10px"
                  :border-style "solid"
                  :border-width "1px"
                  :border-color "white"}}
    [:pre "Ore   :  " (-> game :resources :ore)]
    [:pre "Water :  " (-> game :resources :water)]
    [:pre "Coal  :  " (-> game :resources :coal)]]])

(defmulti render-menu #(-> %1 ::state ::menu) :default :default)
(defmethod render-menu :default
  [game right-space]
  (inner-menu
   (report-menu game right-space)
   (resource-panel game)
   [:div {:style {:padding-bottom "20px"}}
    [:div {:style {:text-decoration "underline"
                   :padding-bottom "20px"
                   :line-height "1"}}
     "Build"]
    (build-button
     game
     "Drill"
     "Harvests 1 ore every 10 seconds."
     [3 0 1])
    (build-button
     game
     "Dehydrator"
     "Harvests 1 water every 10 seconds."
     [3 0 1])
    (build-button
     game
     "Coalescer "
     "Harvests 1 coal every 10 seconds."
     [3 0 1])
    (build-button
     game
     "Light Turret"
     "A horrible machine. Used to murder others."
     [12 5 10])
    (build-button
     game
     "Human Transmutation Machine"
     "A dark, forbidden thing."
     [231 231 231])]))

(defmethod render-menu :purchase [game _]
  (let [report-text (-> game :state :report)]
   [:div
    {:style {:padding "20px"}}
    (resource-panel game)
    (if report-text
     (str
       report-text " "
       (if (= report-text no-purchase-text) ""
           (-> game ::state ::purchase first))))]))

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
    [:div {:style (menu-style x y r d)} (render-menu game r)]
    [:div {:style (text-box-style x y r d)} (render-textbox game)]]))

(defn app []
  (let [game @state]
    (case (-> game ::state ::mode)
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
(defmethod handle-input "Escape"
  [game event]
  (if (purchasing? game)
    (cancel-purchase game)
    game))

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
