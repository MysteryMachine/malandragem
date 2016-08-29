(ns game.core
  (:require [malandragem.core :as rl :refer [px]]))

(enable-console-print!)
(declare state machines report update-machine)

(def color
  {:darkness "#080403"
   :silver "#313235"
   :red "#522"
   :green "#252"})

(def enemy-start [52 74])
(def end [7 38])

(def atlas
  [{:normal [52 48]
    :alt [52 29]}
   {:normal [7 48]
    :alt [7 29]}
   {:normal end
    :alt end}])

(defn move-knight [[x y :as loc] knight]
  (let [[xf yf :as dest] (-> atlas (get (:status knight)) (get (:type knight)))]
    (cond
      (= loc end) nil
      (= loc dest) (move-knight loc (update knight :status inc))
      :else
      (let [x* (js/Math.floor (- xf x))
            y* (js/Math.floor (- yf y))
            dx (if (zero? x*) 0 (js/Math.floor (/ x* (js/Math.abs x*))))
            dy (if (zero? y*) 0 (js/Math.floor (/ y* (js/Math.abs y*))))]
        [[(+ x dx) (+ y dy)] knight]))))

(def spawn-schedule
  (into #{}
        (concat
         (range 30 45 5)
         (range 60 80 5) 
         (range 90 115 5)
         (range 120 135 4)
         (range 150 170 4)
         (range 180 210 4)
         (range 210 225 3)
         (range 240 260 3)
         (range 270 300 3)
         (range 300 315 2)
         (range 330 350 2)
         (range 360 390 2)
         (range 410 1200))))

(def no-purchase-text "Can't place purchase here")

(defn purchasing? [game] (= :purchase(-> game ::state ::menu)))

(defn get-player [game]
  (-> (rl/get-level game) :entities :character first))

(defn cancel-purchase [game]
  (assoc-in game [::state ::menu] nil))

(defn => [& s]
  (let [[style s*] (if (map? (first s))

                     [(first s) (rest s)]
                     [{} s])]
    [:div {:style {:display "flex" :padding-bottom "1em"}}
     [:pre {:style {:margin "0"}} ">  "]
     [:div {:style (merge {:float "left" :display "inline"} style)} (apply str s)]]))

(defn complete-purchase []
  (swap!
   state
   (fn [game]
     (if (not= (-> game :state :report) no-purchase-text)
       (let [[machine tag ore water coal] (-> game ::state ::purchase)
             ore* (-> game :resources :ore)
             water* (-> game :resources :water)
             coal* (-> game :resources :coal)]
        (->
          game
          (update-in
           (rl/level-path game)
           (partial rl/+entity
                    tag
                    (rl/mouse-pos game)
                    (get machines machine)))
          (assoc
           :resources
           {:ore  (- ore* ore)
            :water (- water* water)
            :coal (- coal* coal)})
          (update :text #(conj % (=> "You build the "  machine ".")))
          cancel-purchase))
       game))))

(defn cancel-purchase! []
  (swap! state cancel-purchase))

(defn gather [game resource]
  (update-in game [:resources resource] inc))

(defn floor-gather [game resource coord]
  (if (rl/near? coord (first (get-player game)) 1.8)
    (->
     game
     (gather (keyword resource))
     (update :text #(conj % (=> "You gather "  resource "."))))
    game))

(defn gather! [resource]
  (fn [coord] (swap! state floor-gather resource coord)))

(defn game-tile
  [desc tcolor & {:keys [solid on-click] :as props}]
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
           :on-click (cond p? complete-purchase
                           on-click #(on-click coords)
                           :else nil)
           :on-mouse-over
           (fn []
             ((report
               coords
               (if (purchasing? @state) place-desc desc))))))))))

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

(def heir
  (game-tile
   "The fallen heir."
   "#DDD"
   :solid true
   :name :heir))

(def knight
  (game-tile
   "A noble knight, peacefully attempting to interrupt the ritual."
   "#F88"
   :solid false
   :name :knight
   :status 0
   :health 8))

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

(def transmutation-platform
  (game-tile
   "A glowing transmutation platform"
   "#483939"
   :solid false
   :name :transmutation-platform))

(def water
  (game-tile
   "Murky water. Click to gather water."
   "#226"
   :solid true
   :name :water
   :on-click (gather! "water")))

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
   :solid :walkable
   :name :coal-machine))

(def drill-machine
  (game-tile
   "A machine that drills deeply with lasers."
   "#888"
   :solid :walkable
   :name :drill-machine))

(def dehydrator
  (game-tile
   "Sucks out oxygen from the air."
   "#494"
   :solid :walkable
   :name :dehydrator))

(def turret
  (game-tile
   "A machine used for killing."
   "#F69"
   :solid :walkable
   :name :turret))

(def blood-stain
  (game-tile
   "Human blood."
   "#F00"
   :solid :walkable
   :name :blood))

(def human-trans-machine
  (game-tile
   "You've done it now."
   "#FF0"
   :solid :true
   :name :human-trans-machine
   :on-click (fn [& _] (swap! state #(assoc-in % [::state ::mode] :end)))))

(def coal-pile
  (game-tile
   "A chute containing a large amount of coal. Click to gather coal."
   "#080404"
   :solid true
   :name :coal-pile
   :on-click (gather! "coal")))

(def ore-pile
  (game-tile
   "A tall mound of dusty ores. Click to gather ore."
   "#622"
   :solid true
   :name :ore-pile
   :on-click (gather! "ore")))

(def machines
  {"Coalescer" coal-machine
   "Drill" drill-machine
   "Dehydrator" dehydrator
   "Light Turret" turret
   "Human Transmutation Machine" human-trans-machine})

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
   (rl/blit-room console light-machine 1 [14 17] [6 3] true)
   (rl/blit-rect transmutation-platform 3 34 9 9)
   (rl/blit-rect ore-pile 42 36 5 5)
   (rl/blit-rect coal-pile 14 61 6 3)
   (rl/+entity :heir end heir)))

(defn construct-level []
  (->>
   {:entities
    {:character
     {[17 22] player}
     :machine {}
     :knight {}
     :turret {}}
    :tiles {:default default-tile}}
   (entrance-hallway)
   (main-chambers)
   (hallways)
   (objects)))

(def levels {0 (construct-level)})

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
(defmethod handle-input "a"
  [game event]
  (move-player game -1 0))
(defmethod handle-input "d"
  [game event]
  (move-player game 1 0))
(defmethod handle-input "s"
  [game event]
  (move-player game 0 1))
(defmethod handle-input "w"
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

(defmulti update-entity (fn [_ type coord _] type) :default :default)
(defmethod update-entity :default [game i _ _] (println i) game)
(defmethod update-entity :machine [game _ _ m]
  (if (= 0 (rem (:time game) (* (-> game :settings :time :fps) 10)))
    (gather game (case (:name m)
                   :drill-machine :ore
                   :coal-machine :coal
                   :dehydrator :water))
    game))
(defmethod update-entity :knight [game _ coord knight]
  (if (= :blood (:name knight)) game
   (let [[new-coord new-knight :as a] (move-knight coord knight)]
     (if a
       (rl/update-entity game [:knight coord] new-coord new-knight)
       (assoc-in game [::state ::mode] :lose)))))

(defn damage-knight [game coord knight]
  (let [knight* (update knight :health dec)
        new-knight (if (zero? (:health knight*))
                     blood-stain
                     knight*)]
    (rl/update-entity game [:knight coord] coord new-knight)))

(defmethod update-entity :turret [game _ coord turret]
  (let [targets (rl/near-filter game coord 4 #(= :knight (:name %2)))]
    (if (seq targets)
      (let [[kcoord knight] (first targets)]
        (damage-knight game kcoord knight))
      game)))

(defn update-entities [game]
  (let [etypes (:update-heiarchy game)]
   (loop [game game
          etypes etypes
          entities []]
     (if (seq etypes)
       (if-not (nil? (seq entities))
         (let [[[c e] & remaining] entities]
           (recur (update-entity game (first etypes) c e) etypes remaining))
         (let [etypes* (rest etypes)
               es (rl/entities-of-type game (first etypes*))]
           (recur game etypes* es)))
       game))))

(defn spawn [game]
  (let [time (:time game)]
    (if (spawn-schedule time)
      (update-in
       game
       (rl/level-path game)
       (partial rl/+entity
                :knight
                enemy-start
                (assoc knight :type (case (rand-int 2) 0 :normal 1 :alt))))
      game)))

(defn tick-game! [game keypresses]
  (if (= :game (-> game ::state ::mode))
   (let [events (set (rl/take-all! keypresses))]
     (->
      game
      (handle-inputs events)
      (rl/tick)
      (update-entities)
      (spawn)))
   game))

(def state
  (rl/data
   :levels levels
   ::state {::mode :init ::index 0 ::menu :default ::findex 0}
   :state {:level 0 :viewport [0 14]
           :debug true}
   :text [(=> "The heavy iron door locks in once opened, leaving the facility "
              "exposed to the elements. You hear marching from far away. "
              "You must make haste, and prepare your defenses. Find ore "
              "and feed it to tsshe light machine.")]
   :entity-heiarchy [:blood :machine :turret :heir :knight :character]
   :update-heiarchy [:machine :turret :knight]
   :resources {:water 50 :coal 50 :ore 50}
   :settings {:tile-dimensions [35 17]
              :screen-dimensions [0.85 0.9]
              :time {:fn tick-game! :fps 5}}))

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

(defn purchase [& [name tag ore water coal :as args]]
  (swap! state
         #(->
           %
           (assoc-in [::state ::menu] :purchase)
           (assoc-in [::state ::purchase] args)
           (assoc-in [:state :report] nil))))

(defn build-button [game name tag desc [ore water coal]]
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
           :on-click (when enough? #(purchase name tag ore water coal))
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
     :machine
     "Harvests 1 ore every 10 seconds."
     [5 5 5])
    (build-button
     game
     "Dehydrator"
     :machine
     "Harvests 1 water every 10 seconds."
     [5 0 3])
    (build-button
     game
     "Coalescer"
     :machine
     "Harvests 1 coal every 10 seconds."
     [5 5 1])
    (build-button
     game
     "Light Turret"
     :turret
     "A horrible machine. Used to murder others."
     [12 5 10])
    (build-button
     game
     "Human Transmutation Machine"
     :machine
     "A dark, forbidden thing. Click to transmute."
     [231 231 231])]))

(defmethod render-menu :purchase [game _]
  (let [report-text (-> game :state :report)]
   [:div
    {:style {:padding "20px"}}
    (resource-panel game)
    [:div {:style {:border-style "solid"
                   :border-width "1px"
                   :border-color "white"
                   :color "white"
                   :padding "10px"
                   :margin-bottom "20px"
                   :cursor "pointer"}
           :on-click cancel-purchase!}
     [:div {:style {:cursor "pointer"}} "Cancel"]]
    [:div {:style {:padding-bottom "20px"}}
     (if report-text
        (str
         report-text " "
         (if (= report-text no-purchase-text) ""
             (-> game ::state ::purchase first))))]]))

(defn render-textbox [game]
  (into [:div
         {:style
          {:height "100%"
           :width "100%"
           :overflow-y "scroll"
           :padding "20px"}}]
        (drop (- (count (:text game)) 4) (:text game))))

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

(def end-dialogue
  [[:div
    [:p "With a final flip of the switch, with the cursed machine"
     " built, the lights shown violently."]
    [:p "And the heir rose."]]
   [:div
    [:p "But what was not be expected"]]
   [:div
    [:p "Was the horror the ensued."]]
   [:div
    [:p "Screams only heard once, during the end of an ancient "
     "civilization at its end, were heard once again."]]
   [:div
    [:p
     "And after a little, no screams were heard again."]]])

(defn handle-init-clicks [game]
  (fn []
    (let [index (-> game ::state ::index)]
      (if (< (inc index) (count init-dialogue))
        (swap! state #(update-in % [::state ::index] inc))
        (swap! state #(-> %
                          (assoc-in [::state ::index] 0)
                          (assoc-in [::state ::mode] :game)))))))

(defn handle-end-clicks [game]
  (fn []
    (let [index (-> game ::state ::findex)]
      (if (< (inc index) (count end-dialogue))
        (swap! state #(update-in % [::state ::findex] inc))
        (swap! state #(-> %
                          (assoc-in [::state ::findex] 0)))))))

(defn render-init [game]
  [:div {:style init-style
         :on-click (handle-init-clicks game)}
   [:div {:style inner-init-style}
    [:style "p{ padding-bottom: 2em; }"]
    (get init-dialogue (-> game ::state ::index))]
   [:div {:style click-anywhere-style}
    "-- CLICK ANYWHERE TO CONTINUE --"]])

(defn render-lose [game]
  [:div {:style init-style}
   [:div {:style inner-init-style}
    [:style "p{ padding-bottom: 2em; }"]
    "YOU LOSE"]])

(defn render-final [game]
  [:div {:style init-style
         :on-click (handle-end-clicks game)}
   [:div {:style inner-init-style}
    [:style "p{ padding-bottom: 2em; }"]
    (get end-dialogue (-> game ::state ::findex))]
   (when (< (-> game ::state ::findex) (dec (count end-dialogue)))
     [:div {:style click-anywhere-style}
      "-- CLICK ANYWHERE TO CONTINUE --"])])

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
      :game (render-game game)
      :lose (render-lose game)
      :end (render-final game))))

(def root (. js/document (getElementById "app")))

(rl/wander app state root)

(defn on-js-reload [])
