(ns lambda-sketch.core
  (:import [com.google.common.hash Hashing]))

(defn seed-hash
  [seed]
  (let [h (Hashing/murmur3_128 seed)]
    (fn [x]
      (cond
        ;; TODO: add support for additional types
        (instance? java.lang.Long x)   (bigint (.asLong (.hashLong h x)))
        (instance? java.lang.String x) (bigint (.asLong (.hashUnencodedChars h x)))
        :else (throw (UnsupportedOperationException. (str "TODO:" (class x))))))))

(deftype BloomFilter
  [m bits hashes size]

  clojure.lang.IPersistentCollection
  (cons [this x]
    (BloomFilter. m
                  (reduce (fn [b h] (doto b (.set (h x)))) (.clone bits) hashes)
                  hashes
                  (inc size)))
  (empty [this]
    (BloomFilter. m (java.util.BitSet. m) hashes 0))
  (equiv [this other]
    (and (= m (.m other))
         (.equals bits (.bits other))
         (= (count hashes) (count (.hashes other)))
         (= size (.size other))))

  clojure.lang.Counted
  (count [this] size)

  clojure.lang.IFn
  (invoke [this x] (not (some false? (map (fn [h] (.get bits (h x))) hashes)))))

(defn bloom-filter
  [m k]
  (let [h1 (seed-hash 1)
        h2 (seed-hash 2)]
    (BloomFilter. m
                  (java.util.BitSet. m)
                  (for [i (range k)] (fn [x] (int (mod (+ (h1 x) (* i (h2 x))) m))))
                  0)))

(deftype CountMinSketch
  [arrays hashes size]

  clojure.lang.IPersistentCollection
  (cons [this x]
    (CountMinSketch. (mapv (fn [a h]
                              (let [index (h x)
                                    curr (aget a index)]
                                (doto a (aset index (inc curr)))))
                            (map aclone arrays)
                            hashes)
                     hashes
                     (inc size)))
  (empty [this]
    (let [w (count (first arrays))
          d (count hashes)]
      (CountMinSketch. (repeatedly d #(long-array w)) hashes 0)))
  (equiv [this other]
      (and (every? true? (map #(java.util.Arrays/equals %1 %2) arrays (.arrays other)))
           (= (count hashes) (count (.hashes other)))
           (= size (.size other))))

  clojure.lang.Counted
  (count [this] size)

  clojure.lang.IFn
  (invoke [this x] (apply min (map (fn [a h] (aget a (h x))) arrays hashes))))

(defn count-min-sketch
  [w d]
  (let [h1 (seed-hash 1)
        h2 (seed-hash 2)]
    (CountMinSketch. (repeatedly d #(long-array w))
                     (for [i (range d)] (fn [x] (int (mod (+ (h1 x) (* i (h2 x))) w))))
                     0)))

(deftype HyperLogLog
  [registers]

  clojure.lang.IPersistentCollection
  (cons [this x]
    (let [m (count registers)
          b (int (Math/ceil (/ (Math/log m) (Math/log 2))))
          val (long ((seed-hash 0) x))
          pos (unsigned-bit-shift-right val (- Long/SIZE b))
          remaining (bit-and val (dec (bit-shift-left 1 (- Long/SIZE b))))
          pw (inc (- (Long/numberOfLeadingZeros remaining) b))]
      (HyperLogLog. (update registers pos max pw))))
  (empty [this]
    (let [m (count registers)]
      (HyperLogLog. (vec (repeat m 0)))))
  (equiv [this other]
    (= registers (.registers other)))

  clojure.lang.Counted
  (count [this]
    (let [m (count registers)
          v (count (filter zero? registers))
          correction (/ 0.7213 (inc (/ 1.079 m)))
          e (* correction m m (/ 1 (reduce + (map #(Math/pow 2 (- %)) registers))))]
      (long (if (= v 0) e (* m (Math/log (/ m v))))))))

(defn hyperloglog
  [m]
  (HyperLogLog. (vec (repeat m 0))))

;; (defn hyperloglog-add
;;   [hll x]
;;   (let [m (count (:registers hll))
;;         b (int (Math/ceil (/ (Math/log m) (Math/log 2))))
;;         val (long ((seed-hash 0) x))
;;         pos (unsigned-bit-shift-right val (- Long/SIZE b))
;;         mask (dec (bit-shift-left 1 (- Long/SIZE b)))
;;         remaining (bit-and val mask)
;;         pw (inc (- (Long/numberOfLeadingZeros remaining) b))]
;;     ;; (println x m b)
;;     ;; (println (bstring val) val)
;;     ;; (println (bstring pos) pos)
;;     ;; (println (bstring remaining) remaining)
;;     ;; (println (Long/numberOfLeadingZeros remaining))
;;     (assoc hll
;;            :registers
;;            (update (:registers hll) pos max pw))))

;; (defn hyperloglog-count
;;   [hll]
;;   (let [m (count (:registers hll))
;;         correction (/ 0.7213 (inc (/ 1.079 m)))]
;;     (* correction m m (/ 1 (reduce + (map #(Math/pow 2 (- %)) (:registers hll)))))))
