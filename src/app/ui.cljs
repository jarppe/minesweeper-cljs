(ns app.ui
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [app.game :as game]
   [goog.object :as go]))


(def levels
  {:game.level/easy         {:game/width 9 :game/height 9 :game/mines-count 10}
   :game.level/intermediate {:game/width 16 :game/height 16 :game/mines-count 40}
   :game.level/expert       {:game/width 30 :game/height 16 :game/mines-count 99}})


(def init-state
  {:game/history []
   :game/level   :game.level/easy
   :game/game    (game/make-game (:game.level/easy levels))
   :inter/timer  nil})


(defonce app-state
  (r/atom init-state))


(defn reset-game! []
  (r/rswap! app-state
            (fn [{:as state :inter/keys [timer] :game/keys [game level]}]
              (when timer
                (js/clearTimeout timer))
              (-> state
                  (update :game/history conj game)
                  (assoc :game/game (-> level levels game/make-game))))))


(defn select-level! [level]
  (r/rswap! app-state assoc :game/level level)
  (reset-game!))


(defn level-selector []
  (into [:<>]
        (for [[k _] levels]
          [:button.level-selector
           {:on-click #(select-level! k)}
           (name k)])))


(defn reset-button [{:keys [status]}]
  [:button.reset {:on-click reset-game!}
   (condp = status
     :game.status/win  "😎"
     :game.status/boom "🌚"
     "🌝")])


(defn get-touch-xy [^js evt]
  (let [touch-list (.-targetTouches evt)]
    (if (pos? (.-length touch-list))
      (let [touch-item (.item touch-list 0)]
        [(.-clientX touch-item) (.-clientY touch-item)])
      [0 0])))


(defn get-mouse-xy [^js evt]
  [(.-clientX evt) (.-clientY evt)])


(defn evt-inside? [^js evt {:keys [x1 y1 x2 y2]}]
  (let [[x y] (if (-> evt .-type (= "touchmove"))
                (get-touch-xy evt)
                (get-mouse-xy evt))]
    (and (<= x1 x x2)
         (<= y1 y y2))))


(defn prevent-default [^js evt]
  (.preventDefault evt))


(def flag-delay 200)


(defn make-timer-evt []
  (doto (js/Object.)
    (go/set "type" "timer")
    (go/set "preventDefault" identity)))


(defn client-rect->bounds [^js client-rect]
  {:x1 (.-left client-rect)
   :x2 (.-right client-rect)
   :y1 (.-top client-rect)
   :y2 (.-bottom client-rect)})


(defn get-target-coords [^js target]
  (let [dataset (-> target .-dataset)]
    [(-> dataset .-x (js/parseInt 10))
     (-> dataset .-y (js/parseInt 10))]))


(defn- handle-click-event [^js evt]
  (prevent-default evt)
  (r/rswap! app-state
            (case (.-type evt)
              ; If this was right mouse click, or left click with either ctrl or meta,
              ; toggle the flag without delay. Otherwise start user interaction. Clear 
              ; possible pending timeout, start new timeout and save component bounds 
              ; and coords to state. Also, save the synthetic timer-evt to state, when 
              ; timer elapses we verify that the event is the same, so that possible 
              ; stray timers don't confuse our state management.
              ("mousedown" "touchstart")
              (fn [{:as state :inter/keys [timer] :game/keys [game]}]
                (when timer
                  (js/clearTimeout timer))
                (let [target (.-target evt)
                      timer-evt (make-timer-evt)
                      coords (get-target-coords target)]
                  (if (or (-> evt .-ctrlKey)
                          (-> evt .-metaKey)
                          (-> evt .-button (= 2)))
                    (assoc state
                           :game/game (game/toggle-flag game coords)
                           :inter/timer nil)
                    (assoc state
                           :inter/timer (js/setTimeout handle-click-event flag-delay timer-evt)
                           :inter/timer-evt timer-evt
                           :inter/bounds (-> target (.getBoundingClientRect) (client-rect->bounds))
                           :inter/coords (get-target-coords target)))))

              ; Moving. If we are in user interaction, check if the user is still
              ; within the cell bounds. If not, cancel the interaction.
              ("mousemove" "touchmove")
              (fn [{:as state :inter/keys [timer bounds]}]
                (if (or (nil? timer) (evt-inside? evt bounds))
                  state
                  (do (js/clearTimeout timer)
                      (assoc state :inter/timer nil))))

              ; End of user interaction. If we end up here with timer, it means that 
              ; the interaction was completed within the same DOM element that it 
              ; started, *AND* that interacton ended before the timer was fired. This 
              ; means it's regular click.
              ("mouseup" "touchend")
              (fn [{:as state :inter/keys [timer coords] :game/keys [game]}]
                (if timer
                  (do (js/clearTimeout timer)
                      (assoc state
                             :game/game (game/play game coords)
                             :inter/timer nil))
                  state))

              ; Fired when click has been active for flag-delay milliseconds. This 
              ; means it's a "long" click and we need to toggle the flag. Verify that
              ; the received event is the one saved in state.
              "timer"
              (fn [{:as state :inter/keys [timer timer-evt coords] :game/keys [game]}]
                (if (and (some? timer) (identical? evt timer-evt))
                  (assoc state
                         :game/game (game/toggle-flag game coords)
                         :inter/timer nil)
                  state)))))


(defn- cell [{:keys [cell]}]
  (let [[x y]      (-> cell :cell/coords)
        cell-state (-> cell :cell/state)
        cell-value (-> cell :cell/value)]
    [:div.cell {:class [(when (= cell-state :cell.state/hidden) "hidden")
                        (when (= cell-state :cell.state/flagged) (str "hidden" " " "flagged"))
                        (when (= cell-value :cell.value/boom) "boom")
                        (when (not= cell-value :cell.value/boom)
                          (str "v" cell-value))]
                :data-x (str x)
                :data-y (str y)}
     (case cell-state
       :cell.state/hidden \u2003 ;ZSWP
       :cell.state/flagged "🚩"
       (if (= cell-value :cell.value/boom)
         "🙀"
         (str cell-value)))]))


(defn grid [{:keys [state]}]
  (let [width  (-> state :game/level levels :game/width)
        height (-> state :game/level levels :game/height)
        status (-> state :game/game :game/status)
        board  (-> state :game/game :game/board)]
    [:div.grid {:class [(when (= status :game.status/win) "win")
                        (when (= status :game.status/boom) "boom")]
                :style {:max-width             (-> width (* 2.8) (.toFixed 2) (str "em"))
                        :grid-template-columns (->> "1fr" (repeat width) (str/join " "))}
                :on-touch-start handle-click-event
                :on-touch-move handle-click-event
                :on-touch-end handle-click-event
                :on-mouse-down handle-click-event
                :on-mouse-move handle-click-event
                :on-mouse-up handle-click-event
                :on-click prevent-default
                :on-context-menu prevent-default}
     (for [row (range height)
           col (range width)]
       [cell {:key (str col ":" row)
              :cell (get board [col row])}])]))


(defn game []
  (let [state @app-state]
    [:div
     [:div.controls
      [level-selector]
      [reset-button {:status (-> state :game/game :game/status)}]]
     [grid {:state state}]]))
