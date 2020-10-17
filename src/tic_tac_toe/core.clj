(ns tic-tac-toe.core
  (:gen-class))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input ""))
  ([default]
     (let [input (clojure.string/trim (read-line))]
       (if (empty? input)
         default
         input))))

(defn make-board
  "Makes the board used to play tic-tac-toe"
  ([size](make-board {:size size} 1))
  ([board number]
    (let [size (:size board)]
    (if (<= number (* size size))
      (let [ valued-board (assoc-in board [number :value] "_")]
      (make-board valued-board (inc number)))
      board
    ))
  )
)

(defn render-number
  "Print the number from the board with it's current value"
  [board number]
  (str number,(get-in board [number :value]) "  ")
)

(defn render-row
"Prints the indexes in the board within the range"
[board start]
  (let [size (:size board)]
  (clojure.string/join " " (map render-number (repeat board) (range start (+ start size)))))
)

(defn display-board
  "Displays the board"
  [board]
    (let [size (:size board)]
    (println "\n")
    (println (clojure.string/join "\n\n" 
    (map render-row (repeat board)
          (map #(+ 1 (* % size)) (range 0 size))))))
  
)

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))


(defn make-move
  [board pos string]
  (assoc-in board [pos :value] string))


(defn valid-move?
  [board pos]
  (let [size (:size board)]
    (if (<= pos (* size size))
      (if (= "_" (get-in board [pos :value]))
        true
        false
      )
      false)
  )
)

(defn row-win?
  [board move counter pos]
  (let [size (:size board)]
    (if (<= counter size)
      (if (= move(get-in board [pos :value]))
      (row-win? board move (inc counter) (inc pos))
      false
      )
      true
    )
  )
)


(defn column-win?
  [board move counter pos]
  (let [size (:size board)]
    (if (<= counter size)
      (if (= move(get-in board [pos :value]))
      (column-win? board  move (inc counter) (+ pos size))
      false
      )
      true
    )
  )
)

(defn diagonal-win?
  [board move counter inc_value pos]
  (let [size (:size board)]
    (if (<= counter size)
      (if (= move(get-in board [pos :value]))
      (diagonal-win? board  move (inc counter) inc_value (+ pos (+ size inc_value)))
      false
      )
      true
    )
  )
)

(defn win?
  [board move]
  (let [size (:size board)]
    (if (some true? (map row-win? (repeat board) (repeat move) (repeat 1) (map #(+ 1 (* % size)) (range 0 size ))))
        true
        (if (some true? (map column-win? (repeat board) (repeat move) (repeat 1) (range 1 (inc size) )))
          true
          (if (diagonal-win? board move 1 1 1)
            true
            (if (diagonal-win? board move 1 -1 size)
              true
              false
            )
          )
        )
    )
  )
)

(defn game-over?
  [board]
  (let [size (:size board)]
    (every? true? (map #(not= "_" (get-in board [% :value])) (range 1 (inc (* size size))) ))
  )
)


(defn prompt-move
  [board, player1, player2, move_dict]
  
  (print (str "\n",player1,", it's your turn. Choose a valid position: "))
  (flush)
  (let [pos (parse-int (get-input))]
    (if (valid-move? board pos)
      (do
        (let [move (get move_dict player1) new_board (make-move board pos move)]
          (display-board new_board)
          (if (win? new_board move)
            player1
            (do
              (if (game-over? new_board)
              "draw"
              (prompt-move new_board player2 player1 move_dict)
              )
            )
          )
        )
      )
      (do (println "Invalid move!")
        (prompt-move board player1 player2 move_dict)
      )
    )
  )
  
)

(defn render-tally-record
  [[game_no, result], player1, player2]
  (cond 
    (= result player1) (str game_no,"\t\twon\t\tlost\n")
    (= result player2) (str game_no,"\t\tlost\t\twon\n") 
    :else (str game_no,"\t\tdraw\t\tdraw\n") 
  )
)

(defn render-tally-card
  [tally-card, player1, player2]
  (let [header (str "\n\nGame\t\t",player1,"\t\t",player2,"\n\n")]
  (str header (clojure.string/join "\n\n" (map render-tally-record tally-card (repeat player1) (repeat player2)))))
)

(defn next-steps
  [player1, player2, tally-card]
  (println "\nWhat do you want to do? \n1. Play Again\n2. View tally card\n3. End game")
  (print "Choose a number (1,2,3): ")
  (flush)
  (let [input (get-input "3")]
    (cond
      (= input "1") (do (println (str "\n", player2, " starts this time!!")) input)
      (= input "2") (do 
                      (println (render-tally-card tally-card player1 player2))
                      (next-steps player1 player2 tally-card)
                    )
      (= input "3") (println "Exiting ...")
      :else (do (println "\nWrong input") (next-steps player1 player2 tally-card))
    )
  )
)


(defn update-result
  [game, player1, player2, result, tally-card]
  (cond
    (or (= result player1) (= result player2)) (println (str "\n", result, " has won the game\n"))
    :else (println "It's a draw") 
  )
  (assoc tally-card game result)
)

(defn start-game
  "Marks the beginning of the game"
  ([player1, player2, tally-card] (start-game player1 player2 tally-card 1))
  ([player1, player2, tally-card, game]
    (let [size 3]
      (def move_dict {player1 "X" player2 "O"})
      (def board (make-board size))
    )
    (display-board board)
    (def result (prompt-move board player1 player2 move_dict))
    (def updated-tally-card (update-result game player1 player2 result tally-card))
    (def next-steps-input (next-steps player1 player2 updated-tally-card))
    (if (= "1" next-steps-input)
      (start-game player2 player1 updated-tally-card (inc game))
    )
  )
)





(defn prompt-players
  []
  (println "Who art thou players?\n")
  (print "Player1: ")
  (flush)
  (let [player1 (get-input "player-1")]
    (print "Player2: ")
    (flush)
    (let [player2 (get-input "player-2")]
    (if (= player1 player2)
      (println "\nPlayers need to have different names")
      (do (println (str "\nStarting off the game with ", player1, " and ", player2))
        (start-game player1 player2 {}))
    )
    )
  )
)

(defn -main
  [& args]
  (println "\nTic tac toe, who's gonna bow?\n")
  (prompt-players))
