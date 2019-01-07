(ns vinnik.constants
  (:require [vinnik.bitboard :as bb]
            [vinnik.square :as sq]))

(def ^:const piece-types [:pawns :rooks :knights :bishops :queen :king])

;; Black initial positions
(def ^:const initial-black-pawns (sq/squares ["a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"]))
(def ^:const initial-black-rooks (sq/squares ["a8" "h8"]))
(def ^:const initial-black-knights (sq/squares ["b8" "g8"]))
(def ^:const initial-black-bishops (sq/squares ["c8" "f8"]))
(def ^:const initial-black-queen (sq/squares ["d8"]))
(def ^:const initial-black-king (sq/squares ["e8"]))
(def ^:const initial-black-pieces (concat initial-black-pawns initial-black-rooks
                                          initial-black-knights initial-black-bishops
                                          initial-black-queen initial-black-king))

;; White initial positions
(def ^:const initial-white-pawns (sq/squares ["a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"]))
(def ^:const initial-white-rooks (sq/squares ["a1" "h1"]))
(def ^:const initial-white-knights (sq/squares ["b1" "g1"]))
(def ^:const initial-white-bishops (sq/squares ["c1" "f1"]))
(def ^:const initial-white-queen (sq/squares ["d1"]))
(def ^:const initial-white-king (sq/squares ["e1"]))
(def ^:const initial-white-pieces (concat initial-white-pawns initial-white-rooks
                                          initial-white-knights initial-white-bishops
                                          initial-white-queen initial-white-king))

;; Whole pieces position
(def ^:const initial-whole-pieces (concat initial-black-pieces
                                          initial-white-pieces))

(def ^:const files-bitboards
  (let [files ["a" "b" "c" "d" "e" "f" "g" "h"]
        ranks (range 1 9)]
    (reduce
     (fn [acc file]
       (let [file-squares (map str (repeat file) ranks)
             file-indexes (sq/squares file-squares)
             file-bitboard (bb/bitboard file-indexes)]
         (assoc acc (keyword file) file-bitboard)))
     {}
     files)))

(def ^:const not-files-bitboards
  (reduce-kv
   (fn [m k v]
     (assoc m k (bb/bitboard-not v)))
   {}
   files-bitboards))

(def ^:const ranks-bitboards
  (let [files ["a" "b" "c" "d" "e" "f" "g" "h"]
        ranks (range 1 9)]
    (reduce
     (fn [acc rank]
       (let [rank-squares (map str files (repeat rank))
             rank-indexes (sq/squares rank-squares)
             rank-bitboards (bb/bitboard rank-indexes)]
         (assoc acc (keyword (str rank)) rank-bitboards)))
     {}
     ranks)))

(def ^:const not-ranks-bitboards
  (reduce-kv
   (fn [m k v]
     (assoc m k (bb/bitboard-not v)))
   {}
   ranks-bitboards))
