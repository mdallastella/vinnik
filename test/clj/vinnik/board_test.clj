(ns vinnik.board-test
  (:require [clojure.test :refer :all]
            [vinnik.board :as bb]
            [vinnik.square :as sq]))

(deftest update-composite-bitboard
  (let [board {:white {:pawns (biginteger 1)
                       :rooks (biginteger 2)}}
        updated-board (bb/update-composite-bitboard board :white)]
    (is (= (get-in updated-board [:white :pieces]) (biginteger 3)))))

(deftest update-whole-pieces
  (let [board {:black {:pieces (biginteger 1)}
               :white {:pieces (biginteger 2)}}
        updated-board (bb/update-whole-pieces board)]
    (is (= (:whole-pieces updated-board) (biginteger 3)))))
