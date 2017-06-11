(ns looping-is-recursion)

(defn power [base exp]
  (loop [result 1
         new-exp exp]
    (if (= new-exp 0)
      result
      (recur (* result base) (dec new-exp)))))

(defn last-element [a-seq]
  (loop [elem (first a-seq)
         remaining-seq (rest a-seq)]
    (if (empty? remaining-seq)
      elem
      (recur (first remaining-seq) (rest remaining-seq)))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (not (= (count seq1) (count seq2))) false
        :else (loop [n 0]
                (cond (not (= (nth seq1 n) (nth seq2 n))) false
                      (= (inc n) (count seq1)) true
                      :else (recur (inc n))))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq]
    (cond (empty? s) nil
      (pred (first s)) acc
      :else (recur (inc acc) (rest s)))))

(defn avg [a-seq]
  (loop [sum-of-elems 0
         s a-seq]
    (if (empty? s)
      (/ sum-of-elems (count a-seq))
      (recur (+ sum-of-elems (first s)) (rest s)))))

; using data map
;(defn parity [a-seq]
;  (loop [counts {}
;         s a-seq]
;    (cond (empty? s) (set (keys (filter #(odd? (second %)) counts)))
;          ((complement contains?) counts (first s)) (recur (assoc counts (first s) 1) (rest s))
;          :else (recur (update-in counts [(first s)] inc) (rest s)))))

(defn parity [a-seq]
  (loop [odd-counts #{}
         s a-seq]
    (cond (empty? s) odd-counts
          (contains? odd-counts (first s)) (recur (disj odd-counts (first s)) (rest s))
          :else (recur (conj odd-counts (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [acc 0
         fprev 1
         fcur 0]
    (if (= acc n)
      fcur
      (recur (inc acc) fcur (+ fprev fcur)))))

(defn cut-at-repetition [a-seq]
  [":("])

