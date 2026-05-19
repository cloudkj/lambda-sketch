(ns lambda-sketch.examples.bloom-filter
  (:require [lambda-sketch.core :refer :all]
            ;;[nextjournal.clerk :as clerk]
            ))

(defn visualize-bits [bs max-bit]
  (apply str (map #(if (.get bs %) "1" "_") (range max-bit))))

(doseq [m [100000]
        k (range 5 9)
        n [10000]]
    (let [items (shuffle (range (* 2 n)))
          in (take n items)
          out (take-last n items)
          bf (reduce (fn [bf i] (bloom-filter-add bf i))
                     (bloom-filter m k)
                     in)
          fpr (/ (count (filter #(bloom-filter-test bf %) out)) n)
          expected (Math/pow (- 1 (Math/exp (/ (* (- k) n) m))) k)]
    (println m k n "actual FPR:" (double fpr) "expected:" expected)))
