(ns vinnik.board
  (:require [vinnik.bitboard :as bb]
            [vinnik.constants :as c]))

(def initial-board
  {:black-pawns   (bb/bitboard c/initial-black-pawns)
   :black-rooks   (bb/bitboard c/initial-black-rooks)
   :black-knights (bb/bitboard c/initial-black-knights)
   :black-bishops (bb/bitboard c/initial-black-bishops)
   :black-queen   (bb/bitboard c/initial-black-queen)
   :black-king    (bb/bitboard c/initial-black-king)
   :white-pawns   (bb/bitboard c/initial-white-pawns)
   :white-rooks   (bb/bitboard c/initial-white-rooks)
   :white-knights (bb/bitboard c/initial-white-knights)
   :white-bishops (bb/bitboard c/initial-white-bishops)
   :white-queen   (bb/bitboard c/initial-white-queen)
   :white-king    (bb/bitboard c/initial-white-king)
   :black-pieces  (bb/bitboard c/initial-black-pieces)
   :white-pieces  (bb/bitboard c/initial-white-pieces)
   :whole-pieces  (bb/bitboard c/initial-whole-pieces)})

(defn- update-composite-bitboard
  [current-board bitboard-key bitboard-list]
  (let [bitboards (vals (select-keys current-board bitboard-list))]
    (assoc current-board bitboard-key
           (apply bb/bitboard-or bitboards))))

(defn update-black-pieces
  [board]
  (update-composite-bitboard board :black-pieces c/black-bitboards))

(defn update-white-pieces
  [board]
  (update-composite-bitboard board :white-pieces c/white-bitboards))

(defn update-whole-pieces
  [board]
  (update-composite-bitboard board :whole-pieces c/whole-bitboards))

(defn- board-test-bitboard
  [board bitboard-key index]
  (let [current-board (:whole-pieces board)]
    (bb/bitboard-test
     (bb/bitboard-and current-board
                      (get board bitboard-key))
     index)))

(defn pprint
  [board]
  (let [indexes (range 63 -1 -1)]
    (->> (reduce
          (fn [acc index]
            (cond
              (board-test-bitboard board :black-rooks index) (conj acc "♜")
              (board-test-bitboard board :black-knights index) (conj acc "♞")
              (board-test-bitboard board :black-bishops index) (conj acc "♝")
              (board-test-bitboard board :black-queen index) (conj acc "♛")
              (board-test-bitboard board :black-king index) (conj acc "♚")
              (board-test-bitboard board :black-pawns index) (conj acc "♟")
              (board-test-bitboard board :white-pawns index) (conj acc "♙")
              (board-test-bitboard board :white-rooks index) (conj acc "♖")
              (board-test-bitboard board :white-bishops index) (conj acc "♗")
              (board-test-bitboard board :white-knights index) (conj acc "♘")
              (board-test-bitboard board :white-queen index) (conj acc "♕")
              (board-test-bitboard board :white-king index) (conj acc "♔")
              :else (conj acc " ")))
          []
          indexes)
         (partition 8)
         (map (partial interpose " "))
         (map (partial apply str))
         (run! println))))
