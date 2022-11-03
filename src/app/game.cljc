(ns app.game)


(def levels
  {:game.level/easy         {:game/width 9 :game/height 9 :game/mines-count 10}
   :game.level/intermediate {:game/width 16 :game/height 16 :game/mines-count 40}
   :game.level/expert       {:game/width 30 :game/height 16 :game/mines-count 99}})


(defn gen-mine
  [max-x max-y]
  [(rand-int max-x) (rand-int max-y)])

(defn gen-mines
  "Returns an infinite sequence of generated mines"
  [width height]
  (repeatedly #(gen-mine width height)))

(defn coords-around
  "Returns coordinates that surround given coordinates
   ? ? ?
   ? * ?
   ? ? ?"
  [[x y]]
  (for [xd    (range -1 2)
        yd    (range -1 2)
        :when (not= [xd yd] [0 0])]
    [(+ x xd) (+ y yd)]))

(defn mines-around
  [coords mines]
  (->> (coords-around coords)
       (filter mines)))

(defn calc-value
  [coords mines]
  (if (contains? mines coords)
    :cell.value/boom
    (count (mines-around coords mines))))

(defn calc-grid
  [width height mines]
  (for [col  (range width)
        row  (range height)
        :let [coords [col row]]]
    {:cell/coords coords
     :cell/state  :cell.state/hidden
     :cell/value  (calc-value coords mines)}))

(defn make-game
  [{:game/keys [width height mines-count]}]
  (let [init {:game/status      :game.status/ok
              :game/mines-count mines-count}]
    (->> (gen-mines width height)
         distinct
         (take mines-count)
         set
         (calc-grid width height)
         (map (juxt :cell/coords identity))
         (into {})
         (assoc init :game/board))))

(defn reveal
  [game coords]
  (update-in game [:game/board coords] assoc :cell/state :cell.state/visible))

(defn reveal-around
  [game coords]
  (->> (coords-around coords)
       (filter (:game/board game))
       (reduce reveal game)))

(defn hidden-zeros-around
  [game coords]
  (->> (coords-around coords)
       (filter
        (fn [coords]
          (let [cell (get-in game [:game/board coords])]
            (and
             (zero? (:cell/value cell))
             (= :cell.state/hidden (:cell/state cell))))))))

(defn hidden-cells
  [game]
  (->> game
       :game/board
       vals
       (filter
        (fn [cell]
          (#{:cell.state/hidden :cell.state/flagged} (:cell/state cell))))))

(defn mines
  [game]
  (->> game
       :game/board
       vals
       (filter (fn [cell] (= :cell.value/boom (:cell/value cell))))))

(defn win?
  [game]
  (= (count (hidden-cells game)) (count (mines game))))

(defn toggle-flag
  [game coords]
  (let [lookup {:cell.state/flagged :cell.state/hidden
                :cell.state/hidden  :cell.state/flagged}]
    (update-in game [:game/board coords :cell/state] lookup)))

(defn flag
  [game coords]
  (assoc-in game [:game/board coords :cell/state] :cell.state/flagged))

(defn should-play?
  "Returns false if game is not in playable state or given coords are
  protected from accidental clicking by flagging."
  [game coords]
  (let [cell-state (get-in game [:game/board coords :cell/state])]
    (and (= :game.status/ok (:game/status game))
         (not= :cell.state/flagged cell-state))))

(defn play
  [game coords]
  (if (should-play? game coords)

    (let [game (reveal game coords) ;; Always reveal clicked cell
          v    (get-in game [:game/board coords :cell/value])]

      (cond
        ;; Mine was revealed => reveal all mines and end game with :boom
        (= :cell.value/boom v) (let [mines (map :cell/coords (mines game))]
                                 (-> (reduce reveal game mines)
                                     (assoc :game/status :game.status/boom)))

        ;; Last hidden 'non-mine' was reveled => end game with :win
        (win? game) (let [mines (map :cell/coords (hidden-cells game))]
                      (-> (reduce flag game mines)
                          (assoc :game/status :game.status/win)))

        ;; Zero was revealed => recursively reveal connected zeros
        (zero? v) (let [zeros (hidden-zeros-around game coords)
                        game  (reveal-around game coords)]
                    (reduce play game zeros))

        ;; Non-zero number was revealed => game goes on
        :else game))

    game))

(comment
  (make-game {:game/width 1 :game/height 5 :game/mines-count 1}))
