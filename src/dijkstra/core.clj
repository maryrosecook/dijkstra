(ns dijkstra.core
  (:use [clojure.pprint])
  (:import java.io.File)
  (:require [clojure.string :as str])
  (:require [dijkstra.read-data :as read-data]))

(defn contains-node? [coll id]
  (some #{id} (map (fn [x] (:id x)) coll)))

(defn get-node [coll id]
  (first (filter (fn [x] (= id (-> x :id))) coll)))

(defn consider-neighbour [neighbour current unvisited]
  (let [tentative-length (+ (-> current :distance) (-> neighbour :length))
        neighbour-node (get-node unvisited (-> neighbour :id))]
    (if (< tentative-length (-> neighbour-node :distance))
      (conj (disj unvisited neighbour-node)
            (assoc neighbour-node :distance tentative-length))
      unvisited))) ;; no change

(defn consider-neighbours [neighbours current unvisited]
  (if (empty? neighbours)
    unvisited
    (let [neighbour (first neighbours)]
      (if (contains-node? unvisited (-> neighbour :id))
        (consider-neighbours (rest neighbours) current (consider-neighbour
                                                        neighbour current unvisited)) ;; handle
        (consider-neighbours (rest neighbours) current unvisited))))) ;; skip

(defn consider [node unvisited visited]
  (let [new-unvisited (consider-neighbours (-> node :neighbours) node unvisited)]
    {:unvisited (disj new-unvisited node) :visited (conj visited node)}))

(defn get-best-node [coll]
  (let [best (first (sort-by :distance coll))]
    (if (= read-data/infinity (-> best :distance))
      nil
      best)))

(defn dijkstra [unvisited visited]
  (let [best (get-best-node unvisited)]
    (if (nil? best)
      visited ;; finished
      (let [{new-unvisited :unvisited new-visited :visited} (consider best unvisited visited)]
        (dijkstra new-unvisited new-visited)))))
