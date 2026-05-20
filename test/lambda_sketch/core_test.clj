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
      (is (= 0 (count-min-sketch-get cms 1)))
      (is (= 0 (count-min-sketch-get cms 2)))
      (is (= 0 (count-min-sketch-get cms 100))))))

(deftest test-basic-counting
  (testing "Should accurately track counts threading state functionally."
    (let [cms (count-min-sketch 1000 5)
          cms (reduce count-min-sketch-add
                      cms
                      (repeat 5 11))
          cms (reduce count-min-sketch-add 
                      cms
                      (repeat 3 22))]
      (is (= 5 (count-min-sketch-get cms 11)))
      (is (= 3 (count-min-sketch-get cms 22)))
      (is (= 0 (count-min-sketch-get cms 33))))))