(ns dijkstra.test.with-array-walk
  (:use [dijkstra.with-array-walk])
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

;; consider-neighbour

(deftest test-updates-neighbour-with-better-distance
  (let [current {:id "1" :distance 5}
        neighbour {:id "141", :length 2621}]
    (is (= 2626
           (:distance (get (consider-neighbour neighbour current nodes)
                           "141"))))))

(deftest test-does-not-update-neighbour-with-worse-distance
  (let [current {:id "1" :distance 5}
        neighbour {:id "200", :length 2621}]
    (is (= 20
           (:distance (get (consider-neighbour neighbour current nodes)
                           "200"))))))

;; consider-neighbours

(deftest test-returns-unvisited-when-done
  (let [unvisited {}]
    (is (= unvisited
           (consider-neighbours [] nil unvisited)))))

(deftest test-considers-unvisited-neighbour
  (let [current {:id "1" :distance 5}
        neighbours [{:id "141", :length 2621}]]
    (is (= 2626
           (:distance (get (consider-neighbours neighbours current nodes)
                           "141"))))))

;; get-best-node

(deftest test-get-best-node-when-still-one-left
  (let [nodes {"11" {:id "11" :distance 23}
               "22" {:id "22" :distance 8}
               "3" {:id "3" :distance 7}}]
    (is (= "3"
           (:id (get-best-node (vals nodes)))))))

(deftest test-get-nil-when-none-left-in-coll
  (let [nodes {}]
    (is (= nil
           (get-best-node (vals nodes))))))

(deftest test-get-nil-when-all-infinity
  (let [nodes {"11" {:id "11" :distance read-data/infinity}
               "3" {:id "3" :distance read-data/infinity}}]
    (is (= nil
           (get-best-node (vals nodes))))))

;; dijkstra

(deftest test-consider-moves-node-from-unvisited-to-visited
  (let [start (now)
        unvisited (read-data/get-nodes)
        unvisited-with-prepped-source (assoc-in unvisited ["1" :distance] 0)
        visited (dijkstra unvisited-with-prepped-source {})]

    (is (= [2599 2610 2947 2052 2367 2399 2029 2442 2505 3068]
           (map (fn [x] (:distance (get visited x)))
                ["7" "37" "59" "82" "99" "115" "133" "165" "188" "197"])))
    (println (str "Dijkstra with array walk: " (/ (- (now) start) 1000.0) "s"))))
