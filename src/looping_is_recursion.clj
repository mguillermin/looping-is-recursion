(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc x n]
                 (if (zero? n)
                   acc
                   (recur (* acc x) x (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc x]
                 (if (empty? x)
                   acc
                   (recur (first x) (rest x))))]
  (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc x1 x2]
                 (cond
                  (and (empty? x1) (empty? x2)) acc
                  (empty? x1) false
                  (empty? x2) false
                  (= (first x1) (first x2)) (recur true (rest x1) (rest x2))
                  :else false))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) n
     :else (recur (inc n) (rest s)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         s a-seq]
    (cond
     (empty? s) (/ sum n)
     :else (recur (inc n) (+ sum (first s)) (rest s)))))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s) acc
      (recur (toggle acc (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [n1 0
         n2 1
         current 0]
    (cond
     (= n 0) 0
     (= n 1) 1
     (= n current) n1
     :else (recur n2 (+ n1 n2) (inc current)))))

(defn cut-at-repetition [a-seq]
  (loop [acc '()
         items #{}
         s a-seq]
    (cond
     (empty? s) acc
     (contains? items (first s)) acc
     :else (recur (concat acc [(first s)]) (conj items (first s)) (rest s)))))

