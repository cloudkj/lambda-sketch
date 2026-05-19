(ns lambda-sketch.examples.count-min-sketch
  (:require [lambda-sketch.core :refer :all]))

(doseq [w [200]
        d [4]
        n [1000]]
  (let [items (repeatedly n #(long (rand-int 50)))
        cms (count-min-sketch w d)
        cms (reduce count-min-sketch-add cms items)]
    (println "w" w "d" d "n" n)
    (doseq [[x actual] (sort (frequencies items))]
      (let [estimate (count-min-sketch-get cms x)]
         (println x actual estimate (not (= estimate actual)))))))
