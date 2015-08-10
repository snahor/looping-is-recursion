(ns looping-is-recursion)

(defn power [base exp]
  (let [aux (fn [acc n]
              (if (zero? n)
                acc
                (recur (* acc base) (dec n))))]
    (aux 1 exp)))

(defn last-element [a-seq]
  (let [aux (fn [xs x]
              (if (empty? xs)
                x
                (recur (rest xs) (first xs))))]
    (aux a-seq nil)))

(defn seq= [seq1 seq2]
  (let [aux (fn [equality xs ys]
              (if (or (false? equality)
                      (and (empty? xs) (empty? ys)))
                equality
                (recur (and equality (= (first xs) (first ys)))
                       (rest xs)
                       (rest ys))))]
    (aux (= (count seq1) (count seq2)) seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         xs a-seq]
    (cond
      (empty? xs) nil
      (pred (first xs)) idx
      :else (recur (inc idx) (rest xs)))))

(defn avg [a-seq]
  (loop [n 0
         s 0
         xs a-seq]
    (if (empty? xs)
      (/ s n)
      (recur (inc n) (+ s (first xs)) (rest xs)))))

(defn parity [a-seq]
  (loop [xs a-seq
         ys #{}]
    (if (empty? xs)
      ys
      (recur (rest xs)
             (let [x (first xs)]
               (if (contains? ys x)
                 (disj ys x)
                 (conj ys x)))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         m 0]
    (if (= m n)
      a
      (recur b (+ a b) (inc m)))))

(defn cut-at-repetition [a-seq]
  (loop [xs a-seq
         ys #{}
         zs []]
    (if (or (empty? xs) (contains? ys (first xs)))
      zs
      (recur (rest xs)
             (conj ys (first xs))
             (conj zs (first xs))))))

