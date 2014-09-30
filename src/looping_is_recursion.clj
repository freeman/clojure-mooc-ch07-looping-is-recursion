(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (>= 0 k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper
        (fn [c s]
          (if (empty? s)
            c
            (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [acc s1 s2]
          (let [[f1 & r1] s1
                [f2 & r2] s2]
            (cond
             (and (empty? s1) (empty? s2)) true
             (or (empty? s1) (empty? s2)) false
             (not= f1 f2) false
             :else (recur true r1 r2))))
          ]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) i
     :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [s-count 0
         s-sum 0
         s a-seq]
    (if (empty? s)
      (if (= 0 s-count) nil (/ s-sum s-count))
      (recur (inc s-count) (+ s-sum (first s)) (rest s)))))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [pset #{}
         s a-seq]
    (if (empty? s)
      pset
      (recur (toggle pset (first s)) (rest s)))))

(defn fast-fibo [n]
  (cond
   (<= n 0) 0
   (= n 1) 1
   :else (loop [f-2 0
                f-1 1
                x 2]
           (if (= x n)
             (+ f-2 f-1)
             (recur f-1 (+ f-2 f-1) (inc x))))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         kept []
         s a-seq]
    (if (or (empty? s) (contains? seen (first s)))
      kept
      (recur (conj seen (first s)) (conj kept (first s)) (rest s)))))
