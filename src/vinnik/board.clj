(ns vinnik.board
  (:require [vinnik.bitboard :as bb]
            [vinnik.constants :as c]))

(def initial-board
  {:black {:pawns   (bb/bitboard c/initial-black-pawns)
           :rooks   (bb/bitboard c/initial-black-rooks)
           :knights (bb/bitboard c/initial-black-knights)
           :bishops (bb/bitboard c/initial-black-bishops)
           :queen   (bb/bitboard c/initial-black-queen)
           :king    (bb/bitboard c/initial-black-king)
           :pieces  (bb/bitboard c/initial-black-pieces)}
   :white {:pawns   (bb/bitboard c/initial-white-pawns)
           :rooks   (bb/bitboard c/initial-white-rooks)
           :knights (bb/bitboard c/initial-white-knights)
           :bishops (bb/bitboard c/initial-white-bishops)
           :queen   (bb/bitboard c/initial-white-queen)
           :king    (bb/bitboard c/initial-white-king)
           :pieces  (bb/bitboard c/initial-white-pieces)}
   :whole-pieces (bb/bitboard-or (bb/bitboard c/initial-black-pieces)
                                 (bb/bitboard c/initial-white-pieces))})

(defn update-composite-bitboard
  "Take all the bitboard of one side and update the pieces positions."
  [board color]
  (let [bitboards (-> (get board color)
                      (dissoc :pieces)
                      vals)]
    (assoc-in board [color :pieces]
              (apply bb/bitboard-or bitboards))))

(defn update-whole-pieces
  "Take all the bitboard of one side and update the pieces positions."
  [board]
  (->> (bb/bitboard-or (get-in board [:black :pieces])
                       (get-in board [:white :pieces]))
       (assoc board :whole-pieces)))

(defn- board-test-bitboard
  [board bitboard-key index]
  (let [whole-bitboard (:whole-pieces board)]
    (bb/bitboard-set?
     (bb/bitboard-and whole-bitboard
                      (get-in board bitboard-key))
     index)))

(defn bb-color-piece
  "Return the bitboard of a specific color/piece from the board."
  [board color piece]
  (get-in board [color piece]))

(defn bb-black-piece
  "Return the bitboard for a specific black piece from the board."
  [board piece]
  (bb-color-piece board :black piece))

(defn bb-white-piece
  "Return the bitboard for a specific white piece from the board."
  [board piece]
  (bb-color-piece board :white piece))

(defn pprint
  "Pretty print the chessboard."
  [board]
  (let [indexes (range 63 -1 -1)]
    (->> (reduce
          (fn [acc index]
            (cond
              (board-test-bitboard board [:black :pawns] index) (conj acc "♟")
              (board-test-bitboard board [:black :rooks] index) (conj acc "♜")
              (board-test-bitboard board [:black :knights] index) (conj acc "♞")
              (board-test-bitboard board [:black :bishops] index) (conj acc "♝")
              (board-test-bitboard board [:black :queen] index) (conj acc "♛")
              (board-test-bitboard board [:black :king] index) (conj acc "♚")
              (board-test-bitboard board [:white :pawns] index) (conj acc "♙")
              (board-test-bitboard board [:white :rooks] index) (conj acc "♖")
              (board-test-bitboard board [:white :bishops] index) (conj acc "♗")
              (board-test-bitboard board [:white :knights] index) (conj acc "♘")
              (board-test-bitboard board [:white :queen] index) (conj acc "♕")
              (board-test-bitboard board [:white :king] index) (conj acc "♔")
              :else (conj acc " ")))
          []
          indexes)
         (partition 8)
         (map (partial interpose " "))
         (map (partial apply str))
         (run! println))))
