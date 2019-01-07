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
      d/north))

(defn- pawns-white-double-push
  [pawns-bitboard]
  (let [rank-2 (:2 c/ranks-bitboards)]
    (-> pawns-bitboard
        (bb/bitboard-and rank-2)
        d/north
        d/north)))

(defn- pawns-white-captures
  [pawns-bitboard board]
  (let [not-file-a (:a c/not-files-bitboards)
        not-file-h (:h c/not-files-bitboards)
        black-pieces (b/bb-black-piece board :pieces)
        pawns-west-attacks (d/north-west (bb/bitboard-and pawns-bitboard not-file-a))
        pawns-east-attacks (d/north-east (bb/bitboard-and pawns-bitboard not-file-h))]
    (bb/bitboard-or (bb/bitboard-and pawns-west-attacks black-pieces)
                    (bb/bitboard-and pawns-east-attacks black-pieces))))

(defn- pawns-black-captures
  [pawns-bitboard board]
  (let [not-file-a (:a c/not-files-bitboards)
        not-file-h (:h c/not-files-bitboards)
        white-pieces (b/bb-white-piece board :pieces)
        pawns-west-attacks (d/south-west (bb/bitboard-and pawns-bitboard not-file-a))
        pawns-east-attacks (d/south-east (bb/bitboard-and pawns-bitboard not-file-h))]
    (bb/bitboard-or (bb/bitboard-and pawns-west-attacks white-pieces)
                    (bb/bitboard-and pawns-east-attacks white-pieces))))

(defn- pawns-black-single-push
  [pawns-bitboard]
  (-> pawns-bitboard
      d/south))

(defn- pawns-black-double-push
  [pawns-bitboard]
  (let [rank-7 (:7 c/ranks-bitboards)]
    (-> pawns-bitboard
        (bb/bitboard-and rank-7)
        d/south
        d/south)))

(defmethod pseudo-moves :pawns
  [{color :color board :board :as context}]
  (let [pawns-bitboard (b/bb-color-piece board color :pawns)]
    (condp = color
      :black (assoc-in context [:moves :black :pawns]
                       (bb/bitboard-or
                        (pawns-black-single-push pawns-bitboard)
                        (pawns-black-double-push pawns-bitboard)
                        (pawns-black-captures pawns-bitboard
                                              board)))
      :white (assoc-in context [:moves :white :pawns]
                       (bb/bitboard-or
                        (pawns-white-single-push pawns-bitboard)
                        (pawns-white-double-push pawns-bitboard)
                        (pawns-white-captures pawns-bitboard
                                              board))))))

;; TODO: Distinct knights moves from captures

(defn- knights-moves
  [knights-bitboard]
  (let [not-file-a (:a c/not-files-bitboards)
        not-file-h (:h c/not-files-bitboards)
        not-files-ab (bb/bitboard-and not-file-a (:b c/not-files-bitboards))
        not-files-gh (bb/bitboard-and not-file-h (:g c/not-files-bitboards))]
    (let [l1 (bb/bitboard-and (d/east knights-bitboard) not-file-a)
          l2 (bb/bitboard-and (d/double-east knights-bitboard) not-files-ab)
          r1 (bb/bitboard-and (d/west knights-bitboard) not-file-h)
          r2 (bb/bitboard-and (d/double-west knights-bitboard) not-files-gh)
          h1 (bb/bitboard-or l1 r1)
          h2 (bb/bitboard-or l2 r2)]
      (bb/bitboard-or (.shiftLeft h1 16)
                      (.shiftRight h1 16)
                      (.shiftLeft h2 8)
                      (.shiftRight h2 8)))))

(defmethod pseudo-moves :knights
  [{color :color board :board :as context}]
  (let [knights-bitboard (b/bb-color-piece board color :knights)
        not-occupied-squares (bb/bitboard-not (b/bb-color-piece board color :pieces))
        knights-moves (knights-moves knights-bitboard)]
    (assoc-in context [:moves color :knights] (bb/bitboard-and knights-moves
                                                               not-occupied-squares))))
