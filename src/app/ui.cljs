(ns app.ui
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [app.game :as game]
   [app.state :as state]
   [app.util :as util]))


(def levels [[:game.level/easy "signal_cellular_alt_1_bar"]
             [:game.level/intermediate "signal_cellular_alt_2_bar"]
             [:game.level/expert "signal_cellular_alt"]])


(defn level-selector [{:keys [set-level]}]
  (into [:<>] (for [[level-id icon-name] levels]
                [:a.level-selector {:on-click (fn [_] (set-level level-id))
                                    :href "#"}
                 [:span.material-icons-round.md-24 icon-name]])))


(defn reset-button [{:keys [status reset-game]}]
  [:a.reset {:on-click (fn [e]
                         (util/prevent-default e)
                         (reset-game)
                         nil)
             :href "#"}
   [:span (case status
            :game.status/ok "🌝"
            :game.status/win "😎"
            :game.status/boom "🌚")]])


(defn undo-button [{:keys [disabled? undo]}]
  [:a.undo
   {:class [(when disabled? "disabled")]
    :disabled disabled?
    :on-click (fn [e]
                (util/prevent-default e)
                (undo)
                nil)
    :href "#"}
   [:span.material-icons-round.md-24 "undo"]])


(defn undo-counter [{:keys [undo-counter]}]
  [:div.undo-counter
   (when (pos? undo-counter)
     [:<>
      [:span "Undo used"]
      [:span.count (str undo-counter)]
      [:span "times"]])])


(def flag-delay 100)


(declare handle-click-event)


; If this was right mouse click, or left click with either ctrl or meta,
; toggle the flag without delay. Otherwise start user interaction. Clear 
; possible pending timeout, start new timeout and save component bounds 
; and coords to state. Also, save the synthetic timer-evt to state, when 
; timer elapses we verify that the event is the same, so that possible 
; stray timers don't confuse our state management.

(defn- on-mouse-down [state evt]
  (when-let [timer (-> state :inter/timer)]
    (js/clearTimeout timer))
  (let [target (.-target evt)
        timer-evt (util/make-timer-evt)
        coords (util/get-target-coords target)]
    (if (or (-> evt .-ctrlKey)
            (-> evt .-metaKey)
            (-> evt .-button (= 2)))
      (-> state
          (state/toggle-flag coords)
          (assoc :inter/timer nil))
      (assoc state
             :inter/timer (js/setTimeout handle-click-event flag-delay timer-evt)
             :inter/timer-evt timer-evt
             :inter/bounds (-> target (.getBoundingClientRect) (util/client-rect->bounds))
             :inter/coords (util/get-target-coords target)))))


; Moving. If we are in user interaction, check if the user is still
; within the cell bounds. If not, cancel the interaction.

(defn- on-mouse-move [{:as state :inter/keys [timer bounds]} evt]
  (if (or (nil? timer) (util/evt-inside? evt bounds))
    state
    (do (js/clearTimeout timer)
        (assoc state :inter/timer nil))))


; End of user interaction. If we end up here with timer, it means that 
; the interaction was completed within the same DOM element that it 
; started, *AND* that interacton ended before the timer was fired. This 
; means it's regular click.
;
; If it's a long click and the game has ended (state is not :game.status/ok),
; then make a reset. This saves user from clicking the reset button all the way
; at the top.

(defn- on-mouse-up [{:as state :inter/keys [timer coords]} _evt]
  (if timer
    (do (js/clearTimeout timer)
        (-> state
            (assoc :inter/timer nil)
            (state/play coords)))
    (if (-> state :game/game :game/status (not= :game.status/ok))
      (-> state
          (assoc :inter/timer nil)
          (state/reset-game))
      state)))


; Fired when click has been active for flag-delay milliseconds. This 
; means it's a "long" click and we need to toggle the flag. Verify that
; the received event is the one saved in state.

(defn- on-timer [{:as state :inter/keys [timer timer-evt coords]} evt]
  (if (and (some? timer) (identical? evt timer-evt))
    (-> state
        (state/toggle-flag coords)
        (assoc :inter/timer nil))
    state))


(defn- handle-click-event [^js evt]
  (r/rswap! state/app-state
            (case (.-type evt)
              ("mousedown" "touchstart") on-mouse-down
              ("mousemove" "touchmove")  on-mouse-move
              ("mouseup" "touchend")     on-mouse-up
              "timer"                    on-timer)
            (util/prevent-default evt)))


(def state+value->classes
  (apply hash-map
         (into
          [[:cell.state/hidden :cell.value/boom] ["hidden"]
           [:cell.state/visible :cell.value/boom] ["boom"]
           [:cell.state/boom :cell.value/boom] ["boom"]
           [:cell.state/flagged :cell.value/boom] ["hidden" "flagged"]]
          cat
          (for [i (range 10)]
            [[:cell.state/hidden i] ["hidden"]
             [:cell.state/flagged i] ["hidden" "flagged"]
             [:cell.state/visible i] [(str "v" i)]]))))


(defn- cell [{:keys [cell]}]
  (let [[x y]      (-> cell :cell/coords)
        cell-state (-> cell :cell/state)
        cell-value (-> cell :cell/value)]
    [:div.cell {:class (state+value->classes [cell-state cell-value])
                :data-x (str x)
                :data-y (str y)}
     (case cell-state
       :cell.state/hidden \u2003 ;ZSWP
       :cell.state/flagged "🚩"
       (if (= cell-value :cell.value/boom)
         "🙀"
         (str cell-value)))]))


(defn grid [{:keys [state]}]
  (let [width  (-> state :game/level game/levels :game/width)
        height (-> state :game/level game/levels :game/height)
        status (-> state :game/game :game/status)
        board  (-> state :game/game :game/board)]
    [:div.grid {:class [(when (= status :game.status/win) "win")
                        (when (= status :game.status/boom) "boom")]
                :style {:max-width             (-> width (* 2.8) (.toFixed 2) (str "em"))
                        :grid-template-columns (->> "1fr" (repeat width) (str/join " "))}
                :on-touch-start  handle-click-event
                :on-touch-move   handle-click-event
                :on-touch-end    handle-click-event
                :on-mouse-down   handle-click-event
                :on-mouse-move   handle-click-event
                :on-mouse-up     handle-click-event
                :on-click        util/prevent-default
                :on-context-menu util/prevent-default}
     (for [row (range height)
           col (range width)]
       [cell {:key (str col ":" row)
              :cell (get board [col row])}])]))


(defn reset-game! []
  (r/rswap! state/app-state state/reset-game))


(defn select-level! [level]
  (r/rswap! state/app-state (fn [state]
                              (-> state
                                  (assoc :game/level level)
                                  (state/reset-game)))))


(defn undo! []
  (r/rswap! state/app-state state/undo))


(defn game []
  (let [state @state/app-state]
    [:div.game
     [:div.controls
      [level-selector {:set-level select-level!}]
      [reset-button {:status     (-> state :game/game :game/status)
                     :reset-game reset-game!}]
      [undo-button {:disabled? (-> state :game/history (seq) (nil?))
                    :undo undo!}]
      [undo-counter {:undo-counter (-> state :game/undo-counter)}]]
     [grid {:state state}]]))
