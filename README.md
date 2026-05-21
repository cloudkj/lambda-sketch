# lambda-sketch

Probabilistic data structures and sketching algorithms in Clojure:

* Bloom filter - set membership
* Count-min sketch - frequency counts

### Demo

```clojure
=> (use 'lambda-sketch.core)

;; Load tokenized texts
=> (def alice (map clojure.string/lower-case (clojure.string/split (slurp "https://www.gutenberg.org/cache/epub/11/pg11.txt") #"\W+")))
=> (def pride (map clojure.string/lower-case (clojure.string/split (slurp "https://www.gutenberg.org/cache/epub/1342/pg1342.txt") #"\W+")))

;; Initialize bloom filters as set membership predicates
=> (def alice? (into (bloom-filter 30000 7) alice))
=> (def pride? (into (bloom-filter 70000 7) pride))

;; Frequent words in one text that don't occur in the other
=> (filter (complement alice?) (map first (take-last 100 (sort-by second (frequencies pride)))))
("wickham" "jane" "bingley" "bennet" "mrs" "darcy" "elizabeth" "mr")
=> (filter (complement pride?) (map first (take-last 100 (sort-by second (frequencies alice)))))
("rabbit" "gryphon" "hatter" "mock" "turtle" "queen" "alice")

;; Initialize count-min sketch for frequency estimates
=> (def count-alice (into (count-min-sketch 200 5) alice))

;; Estimated frequencies for most frequent words in text
=> (take-last 10 (sort-by second (frequencies alice)))
(["said" 460] ["you" 471] ["i" 524] ["she" 549] ["it" 607] ["of" 637] ["a" 695] ["to" 811] ["and" 941] ["the" 1839])
=> (map (fn [[w _]] [w (count-alice w)]) (take-last 10 (sort-by second (frequencies alice))))
(["said" 519] ["you" 501] ["i" 550] ["she" 580] ["it" 654] ["of" 673] ["a" 723] ["to" 856] ["and" 1010] ["the" 1873])
```
