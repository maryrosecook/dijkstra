(ns dijkstra.read-data
  (:use [clojure.pprint])
  (:import java.io.File)
  (:require [clojure.string :as str]))

(def infinity 999999999999999999)

(defn parse-edges [edges node]
  (if (empty? edges)
    node
    (let [edge (str/split (first edges) #",")
          new-neighbours (conj (-> node :neighbours)
                               {:id (first edge)
                                :length (Integer/parseInt (second edge))})]
      (parse-edges (rest edges)
                   (assoc node :neighbours new-neighbours)))))

(defn parse-lines [lines nodes]
  (if (empty? lines)
    nodes
    (let [line (str/split (str/replace (first lines) "\r" "") #"\t")
          node (parse-edges (rest line)
                            {:id (first line)
                             :neighbours []
                             :distance infinity
                             :visited false})]
      (parse-lines (rest lines) (conj nodes node)))))

(defn get-nodes []
  (let [data-str (slurp (File. "dijkstraData.txt"))
        lines (str/split data-str #"\n")]
    (parse-lines lines #{})))