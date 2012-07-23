(ns dijkstra.core
  (:require [dijkstra.read-data :as read-data]))

(defn consider-neighbour [neighbour current unvisited]
  (let [tentative-length (+ (-> current :distance) (-> neighbour :length))
        neighbour-node (first (filter (fn [x] (= (-> neighbour :id) (-> x :id)))
                                      unvisited))]
    (if (< tentative-length (-> neighbour-node :distance))
      (conj (disj unvisited neighbour-node)
            (assoc neighbour-node :distance tentative-length))
      unvisited))) ;; no change

(defn consider-neighbours [neighbours current unvisited]
  (if (empty? neighbours)
    unvisited
    (consider-neighbours (rest neighbours) current
                         (if (some #{(-> (first neighbours) :id)}
                                   (map (fn [x] (-> x :id)) unvisited))
                           (consider-neighbour (first neighbours) current unvisited) ;;handle
                           unvisited)))) ;; skip

(defn get-best-node [coll]
  (let [best (first (sort-by :distance coll))]
    (if (= read-data/infinity (-> best :distance)) nil best)))

(defn dijkstra [unvisited visited]
  (let [best (get-best-node unvisited)]
    (if (nil? best)
      visited ;; finished
      (dijkstra (disj (consider-neighbours (-> best :neighbours) best unvisited) best)
                (conj visited best)))))
