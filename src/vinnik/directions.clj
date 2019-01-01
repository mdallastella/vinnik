(ns vinnik.directions)

;; Ray-directions
;; northwest    north   northeast
;;         +7    +8    +9
;;             \  |  /
;; west    -1 <-  0 -> +1    east
;;             /  |  \
;;         -9    -8    -7
;; southwest    south   southeast

(defn shift-north
  [^BigInteger bitboard]
  (.shiftLeft bitboard 8))

(defn shift-east
  [^BigInteger bitboard]
  (.shiftRight bitboard 1))

(defn shift-south
  [^BigInteger bitboard]
  (.shiftRight bitboard 8))

(defn shift-west
  [^BigInteger bitboard]
  (.shiftLeft bitboard 1))

(defn shift-north-east
  [^BigInteger bitboard]
  (.shiftLeft bitboard 7))

(defn shift-north-west
  [^BigInteger bitboard]
  (.shiftLeft bitboard 9))

(defn shift-south-east
  [^BigInteger bitboard]
  (.shiftRight bitboard 9))

(defn shift-south-west
  [^BigInteger bitboard]
  (.shiftRight bitboard 7))