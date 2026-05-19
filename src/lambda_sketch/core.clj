(ns lambda-sketch.core
  (:import [com.google.common.hash Hashing]))

(defn to-bytes
  [obj]
  (with-open [baos (java.io.ByteArrayOutputStream.)
              oos (java.io.ObjectOutputStream. baos)]
    (.writeObject oos obj)
    (.toByteArray baos)))

;; TODO: add support for other types
(defn seed-hash
  [seed]
  (let [h (Hashing/murmur3_128 seed)]
    (fn [x]
      (cond
        ;;(instance? java.lang.Double x) (-> h .newHasher (.putDouble x) .hash .asLong)
        ;;(instance? java.lang.Float x) (-> h .newHasher (.putFloat x) .hash .asLong)
        ;;(instance? java.lang.Integer x) (-> h .newHasher (.putInt x) .hash .asLong)
        (instance? java.lang.Long x) (-> h .newHasher (.putLong x) .hash .asLong bigint)
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
