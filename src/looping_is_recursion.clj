(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc s] 
                 (if (empty? s)
                     acc
                    (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [verdict s1 s2]
                 (cond 
                   (and (empty? s1) (empty? s2)) verdict
                   (or (empty? s1) (empty? s2)) false
                   :else (recur (= (first s1) (first s2)) (rest s1) (rest s2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (if (empty? a-seq)
    nil
    (loop [i   0
             value (first a-seq) 
             s   (rest a-seq)]
        (cond 
          (pred value) i
          (empty? s) nil
          :else (recur (inc i) (first s) (rest s))))))

(defn avg [a-seq]
  (loop [n 0
         total 0
         s a-seq]
    (cond 
      (and (empty? s) (= n 0)) 0
      (empty? s) (/ total n)
      :else (recur (inc n) (+ (first s) total) (rest s)))))

(defn toggle [set elem]
  (if (contains? set elem) 
    (disj set elem)
    (conj set elem)))

(defn parity [a-seq]
  (loop [s #{}
         sq a-seq]
    (if (empty? sq)
      s
      (recur (toggle s (first sq)) (rest sq)))))

(defn fast-fibo [n]
  (loop [fib1 1
         fib2 1
         iter 2]
    (cond
      (= n 0) 0
      (= n 1) 1
      (= iter n) fib2
      :else (recur fib2 (+ fib1 fib2) (inc iter)))))

(defn cut-at-repetition [a-seq]
  (loop [newseq []
         oldseq a-seq
         stuff #{}
         current-elem (first oldseq)]
    (cond
      (empty? oldseq) newseq
      (contains? stuff current-elem) newseq
      :else (recur (conj newseq current-elem)
                   (rest oldseq)
                   (conj stuff current-elem)
                   (first (rest oldseq))))))

