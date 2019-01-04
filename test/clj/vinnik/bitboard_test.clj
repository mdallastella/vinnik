(ns vinnik.bitboard-test
  (:require [vinnik.bitboard :as bb]
            [vinnik.constants :as c]
            [vinnik.square :as sq]
            [clojure.test :refer :all]))

(deftest bitboard-with-no-arguments
  (is (= (bb/bitboard) (biginteger 0))))

(deftest bitboard-with-e4-square
  (is (= (bb/bitboard (sq/squares ["e4"]))
         134217728)))

(deftest bitboard-with-e4-e5-squares
  (is (= (bb/bitboard (sq/squares ["e4" "e5"]))
         34493956096N)))

(deftest bitboard-or-e4-e5
  (let [bitboard-e4 (bb/bitboard (sq/squares ["e4"]))
        bitboard-e5 (bb/bitboard (sq/squares ["e5"]))
        bitboard-e4-e5 (bb/bitboard-or bitboard-e4
                                       bitboard-e5
                                       bb/empty-bitboard)]
    (is (= bitboard-e4-e5
           34493956096N))))

(deftest bitboard-and-e4-e5
  (let [bitboard-e4 (bb/bitboard (sq/squares ["e4"]))
        bitboard-e5 (bb/bitboard (sq/squares ["e5"]))
        bitboard-e4-e5 (bb/bitboard-and bitboard-e4
                                        bitboard-e5
                                        bb/universal-bitboard)]
    (is (= bitboard-e4-e5 0N))))

(deftest bitboard-set?-black-queen
  (let [bitboard-black-queen (bb/bitboard c/initial-black-queen)
        black-queen-index (first (sq/squares ["d8"]))]
    (is (true? (bb/bitboard-set? bitboard-black-queen
                                 black-queen-index)))))

(deftest bitboard-not-universal-eq-empty
  (is (= (bb/bitboard-not bb/universal-bitboard)
         bb/empty-bitboard)))
