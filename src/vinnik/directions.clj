(ns vinnik.directions)

;; Ray-directions
;; northwest    north   northeast
;;         +7    +8    +9
;;             \  |  /
;; west    -1 <-  0 -> +1    east
;;             /  |  \
;;         -9    -8    -7
;; southwest    south   southeast

(defn north
  [^BigInteger bitboard]
  (.shiftLeft bitboard 8))

(defn east
  [^BigInteger bitboard]
  (.shiftRight bitboard 1))

(def double-east
  (comp east east))

(defn south
  [^BigInteger bitboard]
  (.shiftRight bitboard 8))

(defn west
  [^BigInteger bitboard]
  (.shiftLeft bitboard 1))

(def double-west
  (comp west west))

(defn north-west
  [^BigInteger bitboard]
  (.shiftLeft bitboard 9))

(defn north-east
  [^BigInteger bitboard]
  (.shiftLeft bitboard 7))

(defn south-east
  [^BigInteger bitboard]
  (.shiftRight bitboard 9))

(defn south-west
  [^BigInteger bitboard]
  (.shiftRight bitboard 7))
