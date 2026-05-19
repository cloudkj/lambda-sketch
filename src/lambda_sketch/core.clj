(ns lambda-sketch.core
  (:import [com.google.common.hash Hashing]))

(defn seed-hash
  [seed]
  (let [h (Hashing/murmur3_128 seed)]
    (fn [x]
      (cond
        ;; TODO: add support for other types
        (instance? java.lang.Long x) (bigint (.asLong (.hashLong h x)))
        :else (throw (UnsupportedOperationException. (str "TODO:" (class x))))))))

(defn bloom-filter
  [m k]
  ;; For now, underlying data represented as map of internal data structures
  (let [h1 (seed-hash 1)
        h2 (seed-hash 2)]
    {:bits (java.util.BitSet. m)
     :hash-fns (for [i (range k)] (fn [x] (int (mod (+ (h1 x) (* i (h2 x))) m))))}))

(defn bloom-filter-add
  [bf x]
  (assoc bf :bits
         (reduce (fn [b h] (doto b (.set (h x))))
                 (:bits bf)
                 (:hash-fns bf))))

(defn bloom-filter-test
  [bf x]
  (not (some false? (map #(.get (:bits bf) (% x)) (:hash-fns bf)))))
