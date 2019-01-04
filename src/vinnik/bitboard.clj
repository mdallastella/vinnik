(ns vinnik.bitboard)

;; 1 1 1 1 1 1 1 1 56 57 58 59 60 61 62 63
;; 1 1 1 1 1 1 1 1 48 49 50 51 52 53 54 55
;; 0 0 0 0 0 0 0 0 40 41 42 43 44 45 46 47
;; 0 0 0 0 0 0 0 0 32 33 34 35 36 37 38 39
;; 0 0 0 0 0 0 0 0 24 25 26 27 28 29 30 31
;; 0 0 0 0 0 0 0 0 16 17 18 19 20 21 22 23
;; 1 1 1 1 1 1 1 1 08 09 10 11 12 13 14 15
;; 1 1 1 1 1 1 1 1 00 01 02 03 04 05 06 07

(def empty-bitboard BigInteger/ZERO)
(def universal-bitboard (-> (biginteger 2)
                            (.pow 64)
                            dec
                            biginteger))

(defn bitboard-set
  "Sets on the board the bit at index to 1."
  [^BigInteger board index]
  (.setBit board index))

(defn bitboard-unset
  "Sets on the board the bit at index to 0."
  [^BigInteger board index]
  (.clearBit board index))

(defn bitboard
  "Return a bitboard (64bit integer). With arguments, set the bits to 1, without
  arguments returns an empty bitboard (0N)."
  ([]
   (biginteger 0))
  ([indexes]
   (reduce
    (fn [bitboard index]
      (bitboard-set bitboard index))
    (bitboard)
    indexes)))

(defn bitboard-and
  "Perform a bitwise and operation between two or more bitboards."
  ([x y]
   (.and x y))
  ([x y & more]
   (let [all (list* x y more)]
     (reduce
      (fn [acc bb]
        (bitboard-and acc bb))
      universal-bitboard
      all))))

(defn bitboard-or
  "Perform a bitwise or operation between two or more bitboards."
  ([x y]
   (.or x y))
  ([x y & more]
   (let [all (list* x y more)]
     (reduce
      (fn [acc bb]
        (bitboard-or acc bb))
      empty-bitboard
      all))))

(defn bitboard-not
  "Return the complementary bitboard (~bitboard).
  For example, 1001 become 0110."
  [^BigInteger bitboard]
  (->> (range 63 -1 -1)
       (reduce
        (fn [acc index]
          (.flipBit acc index))
        bitboard)))

(defn bitboard-set?
  "Return true if the bit required is set on the bitboard."
  [bitboard index]
  (.testBit bitboard index))

(defn- zero-or-one-test
  [^BigInteger bitboard index]
  (if (bitboard-set? bitboard index)
    "1"
    "0"))

(defn pprint
  "Pretty print the bitboard"
  [^BigInteger bitboard]
  (->> (range 63 -1 -1)
       (map (partial zero-or-one-test bitboard))
       (partition 8)
       (map (partial interpose " "))
       (map (partial apply str))
       (run! println)))
