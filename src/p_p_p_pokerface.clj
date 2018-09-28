(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (Integer/valueOf (replacements rnk)))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn n-of-a-kind? [hand f n]
  (= n (apply f (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (n-of-a-kind? hand max 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand max 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand max 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand)))) (apply min (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (n-of-a-kind? hand max 3) (n-of-a-kind? hand min 2)))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn in? [seq elem]
  (boolean (some #(= % elem) seq)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted (sort ranks)
        sorted-with-low-ace (sort (replace {14 1} sorted))
        min-elem (apply min sorted)
        min-elem-with-low-ace (apply min sorted-with-low-ace)
        expected (range min-elem (+ min-elem 5))
        expected2 (range min-elem-with-low-ace (+ min-elem-with-low-ace 5))]
    (or (= sorted expected)
        (= sorted-with-low-ace expected2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

  (defn value [hand]
    (cond
      (straight-flush? hand) 8
      (four-of-a-kind? hand) 7
      (full-house? hand) 6
      (flush? hand) 5
      (straight? hand) 4
      (three-of-a-kind? hand) 3
      (two-pairs? hand) 2
      (pair? hand) 1
      :else 0))
