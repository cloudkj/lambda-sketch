(ns lambda-sketch.core
  (:import [com.google.common.hash Hashing]))

;; TODO: expand hashing support for Clojure types

(defn seed-hash
  [seed]
  (let [h (Hashing/murmur3_128 seed)]
    (fn [x]
      (cond
        ;; TODO: add support for other types
        (instance? java.lang.Long x) (bigint (.asLong (.hashLong h x)))
        :else (throw (UnsupportedOperationException. (str "TODO:" (class x))))))))

;; TODO: consider implementing data structures as IPersistentCollection for cons, etc

(defn bloom-filter
  [m k]
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

(defn count-min-sketch
  [w d]
  (let [h1 (seed-hash 1)
        h2 (seed-hash 2)]
    {:arrays (repeatedly d #(long-array w))
     :hash-fns (for [i (range d)] (fn [x] (int (mod (+ (h1 x) (* i (h2 x))) w))))}))

(defn count-min-sketch-add
  [cms x]
  (assoc cms :arrays
         (map (fn [a h] (let [index (h x)
                              curr (aget a index)]
                          (doto a (aset index (inc curr)))))
              (:arrays cms)
              (:hash-fns cms))))

(defn count-min-sketch-get
  [cms x]
  (apply min (map (fn [a h] (aget a (h x)))
                  (:arrays cms)
                  (:hash-fns cms))))
