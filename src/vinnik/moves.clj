(ns vinnik.moves
  (:require [vinnik.board :as b]
            [vinnik.bitboard :as bb]
            [vinnik.constants :as c]
            [vinnik.directions :as d]))

(defmulti pseudo-moves :type)

;; TODO: Pawns attacks

(defn- pawns-white-single-push
  [pawns-bitboard]
  (-> pawns-bitboard
      d/shift-north))

(defn- pawns-white-double-push
  [pawns-bitboard]
  (let [rank-2 (:2 c/ranks-bitboards)]
    (-> pawns-bitboard
        (bb/bitboard-and rank-2)
        d/shift-north
        d/shift-north)))

(defn- pawns-white-captures
  [pawns-bitboard board]
  (let [not-file-a (bb/bitboard-not (:a c/files-bitboards))
        not-file-h (bb/bitboard-not (:h c/files-bitboards))
        black-pieces (b/bb-black-piece board :pieces)
        pawns-west-attacks (d/shift-north-west (bb/bitboard-and pawns-bitboard not-file-a))
        pawns-east-attacks (d/shift-north-east (bb/bitboard-and pawns-bitboard not-file-h))]
    (bb/bitboard-or (bb/bitboard-and pawns-west-attacks black-pieces)
                    (bb/bitboard-and pawns-east-attacks black-pieces))))

(defn- pawns-black-captures
  [pawns-bitboard board]
  (let [not-file-a (bb/bitboard-not (:a c/files-bitboards))
        not-file-h (bb/bitboard-not (:h c/files-bitboards))
        white-pieces (b/bb-white-piece board :pieces)
        pawns-west-attacks (d/shift-south-west (bb/bitboard-and pawns-bitboard not-file-a))
        pawns-east-attacks (d/shift-south-east (bb/bitboard-and pawns-bitboard not-file-h))]
    (bb/bitboard-or (bb/bitboard-and pawns-west-attacks white-pieces)
                    (bb/bitboard-and pawns-east-attacks white-pieces))))

(defn- pawns-black-single-push
  [pawns-bitboard]
  (-> pawns-bitboard
      d/shift-south))

(defn- pawns-black-double-push
  [pawns-bitboard]
  (let [rank-7 (:7 c/ranks-bitboards)]
    (-> pawns-bitboard
        (bb/bitboard-and rank-7)
        d/shift-south
        d/shift-south)))

(defmethod pseudo-moves :pawns
  [{color :color board :board :as context}]
  (let [pawns-bitboard (b/bb-color-piece board color :pawns)]
    (cond
      (= color :black)
      (assoc-in context [:moves :black :pawns] (bb/bitboard-or
                                                (pawns-black-single-push pawns-bitboard)
                                                (pawns-black-double-push pawns-bitboard)
                                                (pawns-black-captures pawns-bitboard
                                                                      board)))
      (= color :white)
      (assoc-in context [:moves :white :pawns] (bb/bitboard-or
                                                (pawns-white-single-push pawns-bitboard)
                                                (pawns-white-double-push pawns-bitboard)
                                                (pawns-white-captures pawns-bitboard
                                                                      board))))))
