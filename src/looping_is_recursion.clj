(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [pow exp]
                 (if (zero? exp)
                   pow
                   (recur (* pow base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         b-seq a-seq]
    (cond
      (empty? b-seq) nil
      (pred (first b-seq)) index
      :else (recur (inc index) (rest b-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         number 0
         b-seq a-seq]
    (if (empty? b-seq)
      (/ sum number)
      (recur (+ sum (first b-seq)) (inc number) (rest b-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parityset #{}
         b-seq a-seq]
    (if (empty? b-seq)
      parityset
      (recur (toggle parityset (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [fn-1 0
           fnn 1
           number 1]
      (if (= number n)
        fnn
        (recur fnn (+ fn-1 fnn) (inc number))))))

(defn cut-at-repetition [a-seq]
  (loop [non-rep-set #{}
         result []
         b-seq a-seq]
    (cond
      (empty? b-seq) result
      (contains? non-rep-set (first b-seq)) result
      :else (recur (conj non-rep-set (first b-seq)) (conj result (first b-seq)) (rest b-seq)))))

