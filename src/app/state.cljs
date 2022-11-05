(ns app.state
  (:require
   [reagent.core :as r]
   [app.game :as game]))


(defonce app-state (r/atom {:game/history ()
                            :game/undo-counter 0
                            :game/level :game.level/easy
                            :game/game (-> :game.level/easy game/levels (game/make-game))
                            :inter/timer nil}))


(defn reset-game [{:as state :inter/keys [timer] :game/keys [level]}]
  (when timer
    (js/clearTimeout timer))
  (-> state
      (assoc :inter/timer nil
             :game/history ()
             :game/undo-counter 0
             :game/game (-> level game/levels (game/make-game)))))


(defn toggle-flag [state coords]
  (-> state
      (update :game/history conj (:game/game state))
      (update :game/game game/toggle-flag coords)))


(defn play [state coords]
  (-> state
      (update :game/history conj (:game/game state))
      (update :game/game game/play coords)))


(defn undo [state]
  (let [[saved-game & history] (-> state :game/history)]
    (if saved-game
      (-> state
          (assoc :game/game saved-game
                 :game/history history)
          (update :game/undo-counter inc))
      state)))


;(game/make-game (:game.level/easy levels))