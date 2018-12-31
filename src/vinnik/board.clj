(ns vinnik.board
  (:require [vinnik.bitboard :as bb]
            [vinnik.square :as sq]))

;; Black initial positions
(defonce initial-black-pawns (sq/squares ["a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"]))
(defonce initial-black-rooks (sq/squares ["a8" "h8"]))
(defonce initial-black-knights (sq/squares ["b8" "g8"]))
(defonce initial-black-bishops (sq/squares ["c8" "f8"]))
(defonce initial-black-queen (sq/squares ["d8"]))
(defonce initial-black-king (sq/squares ["e8"]))
(defonce initial-black-pieces (concat initial-black-pawns initial-black-rooks
                                      initial-black-knights initial-black-bishops
                                      initial-black-queen initial-black-king))
(defonce black-bitboards [:black-pawns :black-rooks
                          :black-knights :black-bishops
                          :black-queen :black-king])

;; White initial positions
(defonce initial-white-pawns (sq/squares ["a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"]))
(defonce initial-white-rooks (sq/squares ["a1" "h1"]))
(defonce initial-white-knights (sq/squares ["b1" "g1"]))
(defonce initial-white-bishops (sq/squares ["c1" "f1"]))
(defonce initial-white-queen (sq/squares ["d1"]))
(defonce initial-white-king (sq/squares ["e1"]))
(defonce initial-white-pieces (concat initial-white-pawns initial-white-rooks
                                      initial-white-knights initial-white-bishops
                                      initial-white-queen initial-white-king))
(defonce white-bitboards [:white-pawns :white-rooks
                          :white-knights :white-bishops
                          :white-queen :white-king])
;; Whole pieces position
(defonce initial-whole-pieces (concat initial-black-pieces initial-white-pieces))
(defonce whole-bitboards (concat black-bitboards white-bitboards))

(def initial-board
  {:black-pawns   (bb/bitboard initial-black-pawns)
   :black-rooks   (bb/bitboard initial-black-rooks)
   :black-knights (bb/bitboard initial-black-knights)
   :black-bishops (bb/bitboard initial-black-bishops)
   :black-queen   (bb/bitboard initial-black-queen)
   :black-king    (bb/bitboard initial-black-king)
   :white-pawns   (bb/bitboard initial-white-pawns)
   :white-rooks   (bb/bitboard initial-white-rooks)
   :white-knights (bb/bitboard initial-white-knights)
   :white-bishops (bb/bitboard initial-white-bishops)
   :white-queen   (bb/bitboard initial-white-queen)
   :white-king    (bb/bitboard initial-white-king)
   :black-pieces  (bb/bitboard initial-black-pieces)
   :white-pieces  (bb/bitboard initial-white-pieces)
   :whole-pieces  (bb/bitboard initial-whole-pieces)})

(defn- update-composite-bitboard
  [current-board bitboard-key bitboard-list]
  (let [bitboards (vals (select-keys current-board bitboard-list))]
    (assoc current-board bitboard-key
           (apply bb/bitboard-or bitboards))))

(defn update-black-pieces
  [board]
  (update-composite-bitboard board :black-pieces black-bitboards))

(defn update-white-pieces
  [board]
  (update-composite-bitboard board :white-pieces white-bitboards))

(defn update-whole-pieces
  [board]
  (update-composite-bitboard board :whole-pieces whole-bitboards))

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
