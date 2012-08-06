(ns fibonacci-heap.core
  (:require [clojure.zip :as z])
  (:use [clojure.pprint]))

(declare update-min find-min create-zipper)

(defn- mark [loc]
  (z/edit loc #(assoc % :marked true)))

(defn root-loc [loc]
  (if (= :end (loc 1))
    loc
    (let [p (z/up loc)]
      (if p
        (recur p)
        loc))))

(defn- find-min-loc [heap]
  (let [minimum-pointer (-> heap :minimum-pointer)]
    (if minimum-pointer
      (reduce (fn [node _] (-> node z/right))
              (-> heap :trees z/down)
              (range minimum-pointer))
      nil)))

(defn- degree [loc]
  (-> loc z/children count))

(defn heap-or-tree-root? [loc]
  (or (nil? (-> loc z/up))
      (nil? (-> loc z/up z/node :key))))

(defn promote-to-root [loc new-roots]
  (reduce z/append-child
          (root-loc loc)
          (map (fn [x] (assoc x :marked false)) new-roots)))

(defn search-proceed [loc key]
  (and (not= nil loc)
       (if (<= (-> loc z/node :key) key)
         loc)))

(defn search-next [loc filter-fn key]
  (if (filter-fn (-> loc z/node))
    loc
    (let [next-loc (or
                    (loop [p (z/down loc)]
                      (if p
                        (or (search-proceed p key)
                            (recur (z/right p)))))
                    (z/right loc)
                    (loop [p loc]
                      (if (z/up p)
                        (or (z/right (z/up p))
                            (recur (z/up p)))
                        nil)))]
      (if (nil? next-loc)
        nil
        (search-next next-loc filter-fn key)))))

(defn- balance-node
  ([loc] (balance-node loc []))
  ([loc roots]
     (let [new-roots (conj roots (z/node loc))
           parent-loc (if (z/left loc)
                        (-> loc z/remove z/up)
                        (-> loc z/remove))]
       (if (:marked (z/node parent-loc))
         (recur parent-loc new-roots)
         (let [final-parent (if (heap-or-tree-root? parent-loc)
                              parent-loc
                              (mark parent-loc))]
           (promote-to-root final-parent new-roots))))))

(defn decrease-key [heap loc new-key]
  (let [edited-loc (z/edit
                    loc
                    (fn [node]
                      (assoc node :key new-key)))
        parent (-> edited-loc z/up)]
    (if (heap-or-tree-root? edited-loc)
      (update-min (assoc heap :trees (root-loc edited-loc)))
      (if (> (-> parent z/node :key)
             (-> edited-loc z/node :key))
        (let [balanced-tree (balance-node edited-loc)
              updated-heap (assoc heap :trees balanced-tree)
              cur-key (:key (find-min updated-heap))]
          (if (< new-key cur-key)
            ;; must call update-min because cannot now find decreased key node
            (update-min updated-heap)
            updated-heap))
        (assoc heap :trees (root-loc edited-loc))))))

(defn- loc-gt [x y]
  (> (-> x z/node :key) (-> y z/node :key)))

(defn- merge-degrees [roots-by-degree root]
  (if-let [x (get roots-by-degree (degree root))]
    (recur
     (dissoc roots-by-degree (degree root))
     (if (loc-gt root x)
       (z/append-child x (z/node root))
       (z/append-child root (z/node x))))
    (assoc roots-by-degree (degree root) root)))

(defprotocol IFibonacciHeap
  (heap-merge [this data key])
  (find-min [this])
  (extract-min [this])
  (search [this filter-fn key])
  (update-min [this])) ;; should be private, really

(defrecord FibonacciHeap [minimum-pointer trees]
  IFibonacciHeap
  (heap-merge [this data key]
    (update-min (assoc this :trees
                       (z/insert-child trees
                                       {:data data :key key :marked false}))))

  (find-min [this]
    (let [min-loc (find-min-loc this)]
      (if min-loc
        (z/node min-loc))))

  (update-min [this]
    (let [[idx _] (reduce (fn [[s-idx smallest] [x-idx x]]
                            (if (< (:key x) (:key smallest))
                              [x-idx x]
                              [s-idx smallest]))
                          (map vector (range)
                               (z/children trees)))]
      (assoc this :minimum-pointer idx)))

  (extract-min [this]
    "Get node with smallest key."
    (let [min (find-min-loc this)
          childless-loc (promote-to-root (-> this :trees) (-> min z/children))
          childless-loc-heap (assoc this :trees childless-loc)
          tree-without-min (root-loc (z/remove (find-min-loc childless-loc-heap)))
          roots (map (fn [x] (create-zipper x))
                     (z/children tree-without-min))
          new-roots (map z/node
                         (vals (reduce merge-degrees {} roots)))]
      (if (> (count new-roots) 0)
        (update-min (assoc this :trees
                           (promote-to-root (create-zipper) new-roots)))
        (assoc (assoc this :trees (create-zipper))
          :minimum-pointer nil))))

  (search [this filter-fn key]
    "Return zipper loc of first node for which passed fn returns true.
     Fibonacci Heaps are not designed for efficient search.  Beware."
    (search-next (-> this :trees z/down) filter-fn key))
  )

(defn create-zipper
  ([]
     (create-zipper {:children []}))
  ([root]
     (z/zipper (constantly true)
               :children
               (fn [node new-children]
                 (assoc node :children new-children))
               root)))

(defn create-heap []
  (FibonacciHeap. nil (create-zipper)))

(defn -main [& args])