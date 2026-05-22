(ns lambda-sketch.core-test
  (:require [clojure.test :refer :all]
            [lambda-sketch.core :refer :all]))

(deftest test-bloom-filter-empty
  (testing "An empty Bloom filter should return false for all queries"
    (let [bf (bloom-filter 100 5)]
      (is (false? (bf 1)))
      (is (false? (bf 2)))
      (is (false? (bf 100))
      (is (= 0 (count bf)))))))

(deftest test-bloom-filter-add
  (testing "Elements added to the filter should always return true, i.e. no false negatives"
    (let [bf (into (bloom-filter 100 5) [1 "foo" "bar" 2 100])]
      (is (true? (bf 1)))
      (is (true? (bf 2)))
      (is (true? (bf 100)))
      (is (true? (bf "foo")))
      (is (true? (bf "bar"))))))

(deftest test-count-min-sketch-empty
  (testing "An empty Count-Min Sketch should return 0 for all queries"
    (let [cms (count-min-sketch 100 5)]
      (is (= 0 (cms 1)))
      (is (= 0 (cms 2)))
      (is (= 0 (cms 100))))))

(deftest test-count-min-sketch-add
  (testing
    (let [cms (count-min-sketch 1000 5)
          cms (into cms (repeat 5 11))
          cms (into cms (repeat 3 22))]
      (is (= 5 (cms 11)))
      (is (= 3 (cms 22)))
      (is (= 0 (cms 33))))))

(deftest test-hyperloglog-empty
  (testing
    (let [hll (hyperloglog 128)]
      (is (= 0 (count hll))))))
