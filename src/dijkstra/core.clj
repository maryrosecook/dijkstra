(ns dijkstra.core
  (:require [dijkstra.read-data :as read-data]))

(defn consider-neighbour [neighbour current unvisited]
  (let [tentative-length (+ (-> current :distance) (-> neighbour :length))
        neighbour-node (get unvisited (-> neighbour :id))]
    (if (< tentative-length (-> neighbour-node :distance))
      (assoc-in unvisited [(-> neighbour :id) :distance] tentative-length)
      unvisited))) ;; no change

(defn consider-neighbours [neighbours current unvisited]
  (if (empty? neighbours)
    unvisited
    (consider-neighbours (rest neighbours) current
                         (if (get unvisited (-> (first neighbours) :id))
                           (consider-neighbour (first neighbours) current unvisited) ;; handle
                           unvisited)))) ;; skip

(defn get-best-node [coll]
  (let [best (first (sort-by :distance coll))]
    (if (= read-data/infinity (-> best :distance)) nil best)))

(defn dijkstra [unvisited visited]
  (let [best (get-best-node (vals unvisited))]
    (if (nil? best)
      visited ;; finished
      (dijkstra (dissoc (consider-neighbours (-> best :neighbours) best unvisited)
                        (-> best :id))
                (assoc visited (-> best :id) best)))))
