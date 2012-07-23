(ns dijkstra.test.core
  (:use [dijkstra.core])
  (:use [clojure.test])
  (:use [clojure.pprint])
  (:require [dijkstra.read-data :as read-data])
  (:require [clojure.set :as clojure-set]))

(def nodes
 #{{:id "199",
    :neighbours
    [{:id "141", :length 2621}
     {:id "200", :length 9430}],
    :distance read-data/infinity}
   {:id "141",
    :neighbours
    [{:id "152", :length 2697}
     {:id "154", :length 5786}],
    :distance read-data/infinity}
   {:id "200",
   :neighbours
    [{:id "108", :length 9976}
     {:id "103", :length 6851}]
    :distance 20}})

(defn now [] (.getTime (new java.util.Date)))

;; contains-node?

(deftest test-does-contains-node?
  (is (contains-node? nodes "199")))

(deftest test-does-not-contains-node?
  (is (not (contains-node? nodes "288"))))

(deftest test-does-not-contains-node-if-has-neighbour?
  (is (not (contains-node? nodes "108"))))

;; get-node

(deftest test-can-get-node
  (is (= "199" (:id (get-node nodes "199")))))

(deftest test-get-node-returns-nil-for-non-existent-node
  (is (nil? (get-node nodes "123234"))))

;; consider-neighbour

(deftest test-updates-neighbour-with-better-distance
  (let [current {:id "1" :distance 5}
        neighbour {:id "141", :length 2621}]
    (is (= 2626
           (:distance (get-node (consider-neighbour neighbour current nodes)
                                "141"))))))

(deftest test-does-not-update-neighbour-with-worse-distance
  (let [current {:id "1" :distance 5}
        neighbour {:id "200", :length 2621}]
    (is (= 20
           (:distance (get-node (consider-neighbour neighbour current nodes)
                                "200"))))))

;; consider-neighbours

(deftest test-returns-unvisited-when-done
  (let [unvisited #{}]
    (is (= unvisited
           (consider-neighbours [] nil unvisited)))))

(deftest test-considers-unvisited-neighbour
  (let [current {:id "1" :distance 5}
        neighbours [{:id "141", :length 2621}]]
    (is (= 2626
           (:distance (get-node (consider-neighbours neighbours current nodes)
                                "141"))))))

;; get-best-node

(deftest test-get-best-node-when-still-one-left
  (let [nodes #{{:id "11" :distance 23} {:id "22" :distance 8} {:id "3" :distance 7}}]
    (is (= "3"
           (:id (get-best-node nodes))))))

(deftest test-get-nil-when-none-left-in-coll
  (let [nodes #{}]
    (is (= nil
           (get-best-node nodes)))))

(deftest test-get-nil-when-all-infinity
  (let [nodes #{{:id "11" :distance read-data/infinity}
                {:id "3" :distance read-data/infinity}}]
    (is (= nil
           (get-best-node nodes)))))

;; dijkstra

(deftest test-consider-moves-node-from-unvisited-to-visited
  (let [start (now)
        unvisited (read-data/get-nodes)
        source (get-node unvisited "1")
        prepped-source (assoc source :distance 0)
        unvisited-with-prepped-source (conj (disj unvisited source) prepped-source)
        visited (dijkstra unvisited-with-prepped-source #{})]

    (is (= [2599 2610 2947 2052 2367 2399 2029 2442 2505 3068]
           (map (fn [x] (:distance (get-node visited x)))
                ["7" "37" "59" "82" "99" "115" "133" "165" "188" "197"])))
    (println (str "Completed Coursera test in: " (/ (- (now) start) 1000.0) "s"))))
