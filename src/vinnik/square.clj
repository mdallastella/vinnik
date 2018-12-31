(ns vinnik.square)

(defn- square
  "Transform a square like `e4` into a index for a 64 array."
  [square]
  (let [[file rank] square
        file-n (- 104 (int file))
        rank-n (Integer/parseInt (str rank))]
    (-> rank-n
        dec
        (* 8)
        (+ file-n))))

(defn squares
  [squares]
  (map square squares))
