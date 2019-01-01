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
  ([^BigInteger bitboard]
   (.shiftLeft bitboard 8)))

(defn shift-east
  ([^BigInteger bitboard]
   (.shiftRight bitboard 1)))

(defn shift-south
  ([^BigInteger bitboard]
   (.shiftRight bitboard 8)))

(defn shift-west
  ([^BigInteger bitboard]
   (.shiftLeft bitboard 1)))

(def shift-north-east
  (comp shift-north shift-east))

(def shift-north-west
  (comp shift-north shift-west))

(def shift-south-east
  (comp shift-south shift-east))

(def shift-south-west
  (comp shift-south shift-west))
