(ns dijkstra.test.with-fibonacci-heap
  (:use [dijkstra.with-fibonacci-heap])
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:require [dijkstra.read-data :as read-data])
  (:require [clojure.set :as clojure-set]))

(def nodes
  {"199" {:id "199",
          :neighbours
          [{:id "141", :length 2621}
           {:id "200", :length 9430}],
          :distance read-data/infinity}
   "141" {:id "141",
          :neighbours
          [{:id "152", :length 2697}
           {:id "154", :length 5786}],
          :distance read-data/infinity}
   "200" {:id "200",
          :neighbours
          [{:id "108", :length 9976}
           {:id "103", :length 6851}]
          :distance 20}})

(defn now [] (.getTime (new java.util.Date)))

;; dijkstra

(deftest test-all
  (let [start (now)
        nodes (read-data/get-nodes)
        unvisited (reduce (fn [acc k]
                            (assoc acc k
                                   (clojure.set/rename-keys (get nodes k) {:distance :key})))
                          {} (keys nodes))
        unvisited-with-prepped-source (assoc-in unvisited ["1" :key] 0)
        visited (start-dijkstra unvisited-with-prepped-source)]
    (is (= [2599 2610 2947 2052 2367 2399 2029 2442 2505 3068]
           (map (fn [x] (:key (get visited x)))
                ["7" "37" "59" "82" "99" "115" "133" "165" "188" "197"])))
    (println (str "Dijkstra with Fibonacci heap: " (/ (- (now) start) 1000.0) "s"))))
