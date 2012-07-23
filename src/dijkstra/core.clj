(ns dijkstra.core
  (:use [clojure.pprint])
  (:import java.io.File)
  (:require [clojure.string :as str])
  (:require [dijkstra.read-data :as read-data]))

(defn contains-node? [coll id]
  (some #{id} (map (fn [x] (-> x :id)) coll)))

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
    (consider-neighbours (rest neighbours) current
                         (if (contains-node? unvisited (-> (first neighbours) :id))
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
