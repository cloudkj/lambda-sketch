(ns lambda-sketch.core-test
  (:require [clojure.test :refer :all]
            [lambda-sketch.core :refer :all]))

(deftest test-empty
  (testing "An empty Bloom filter should return false for all queries"
    (let [bf (bloom-filter 100 5)]
      (is (false? (bloom-filter-test bf 1)))
      (is (false? (bloom-filter-test bf 2)))
      (is (false? (bloom-filter-test bf 100))))))

(deftest test-add
  (testing "Elements added to the filter should always return true, i.e. no false negatives"
    (let [bf (-> (bloom-filter 100 5)
                 (bloom-filter-add 1)
                 (bloom-filter-add 2)
                 (bloom-filter-add 100))]
      (is (true? (bloom-filter-test bf 1)))
      (is (true? (bloom-filter-test bf 2)))
      (is (true? (bloom-filter-test bf 100))))))
