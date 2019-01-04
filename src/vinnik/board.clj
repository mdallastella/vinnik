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

(defn- update-composite-bitboard
  [current-board color]
  (let [bitboards (-> (get current-board color)
                      (dissoc :pieces)
                      vals)]
    (assoc-in current-board [color :pieces]
              (apply bb/bitboard-or bitboards))))

(defn update-black-pieces
  [board]
  (update-composite-bitboard board :black))

(defn update-white-pieces
  [board]
  (update-composite-bitboard board :white))

(defn update-whole-pieces
  [board]
  (bb/bitboard-or (get-in board [:black :pieces])
                  (get-in board [:white :pieces])))

(defn- board-test-bitboard
  [board bitboard-key index]
  (let [current-board (:whole-pieces board)]
    (bb/bitboard-set?
     (bb/bitboard-and current-board
                      (get board bitboard-key))
     index)))

(defn pprint
  [board]
  (let [indexes (range 63 -1 -1)]
    (->> (reduce
          (fn [acc index]
            (cond
              (board-test-bitboard board [:black :rooks] index) (conj acc "♜")
              (board-test-bitboard board [:black :knights] index) (conj acc "♞")
              (board-test-bitboard board [:black :bishops] index) (conj acc "♝")
              (board-test-bitboard board [:black :queen] index) (conj acc "♛")
              (board-test-bitboard board [:black :king] index) (conj acc "♚")
              (board-test-bitboard board [:black :pawns] index) (conj acc "♟")
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
