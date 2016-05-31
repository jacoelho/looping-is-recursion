(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn[acc k]
                 (if (zero? k)
                   acc
                   (recur (* base acc) (dec k))))]
    (helper 1 exp)))

(defn last-element [[x & xs]]
  (let [helper (fn[elem a-seq]
                 (if (empty? a-seq)
                   elem
                   (recur (first a-seq) (rest a-seq))))]
    (helper x xs)))

(defn seq= [seq1 seq2]
  (let [helper (fn [l1 l2]
                 (cond
                   (and (empty? l1) (empty? l2)) true
                   (or  (empty? l1) (empty? l2)) false
                   (= (first l1) (first l2)) (recur (rest l1) (rest l2))
                   :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         lst a-seq]
    (cond
      (empty? lst) nil
      (pred (first lst)) idx
      :else (recur (inc idx) (rest lst)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [acc 0
           cnt 0
           lst a-seq]
      (if (empty? lst)
        (/ acc cnt)
        (recur (+ acc (first lst)) (inc cnt) (rest lst))))))


(defn parity [a-seq]
  (let [toggle (fn[x el]
                (if (contains? x el)
                  (disj x el)
                  (conj x el)))]
    (loop [acc #{}
           lst a-seq]
      (if (empty? lst)
        acc
        (recur (toggle acc (first lst)) (rest lst))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         cnt n]
    (if (zero? cnt)
      a
      (recur b (+ a b) (dec cnt)))))

(defn cut-at-repetition [a-seq]
  (loop [check #{}
         result []
         [x & xs :as lst] a-seq]
    (if
      (or (empty? lst)
          (contains? check x)) result
      (recur (conj check x) (conj result x) xs))))

