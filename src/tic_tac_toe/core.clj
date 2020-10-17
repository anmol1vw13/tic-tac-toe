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
    true
    false)
  )
)


(defn prompt-move
  [board, player1, player2, move_dict]
  
  (println (str player1,", it's your turn. Choose a valid position"))
  (let [pos (parse-int (get-input))]
    (if (valid-move? board pos)
      (do
        (let [new_board (make-move board pos (get move_dict player1))]
        (display-board new_board)
        (prompt-move new_board player2 player1 move_dict))
        
      )
      (do (println "Invalid move!")
        (prompt-move board player1 player2 move_dict)
      )
    )
  )
  
)

(defn start-game
  "Marks the beginning of the game"
  [player1, player2]
  
    (let [size 3]
      (def move_dict {player1 "X" player2 "O"})
      (def board (make-board size))
    )
    (display-board board)
    (prompt-move board player1 player2 move_dict)

)

(defn prompt-players
  []
  (println "Who art thou players?")
  (println "Player1: ")
  (let [player1 (get-input "player-1")]
    (println "Player2: ")
    (let [player2 (get-input "player-2")]
    (if (= player1 player2)
      (println "Players need to have different names")
      (do (println (str "Starting off the game with ", player1, " and ", player2, "\n\n"))
        (start-game player1 player2))
    )
    )
  )
)

  


(defn -main
  [& args]
  (println "Tic tac toe, who's gonna bow?")
  (prompt-players))
