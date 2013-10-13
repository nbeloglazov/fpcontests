(ns rocketscience.segment-tree)

(defn create [from to]
  (let [tree {:from from
              :to to
              :max 0}]
      (if (= from to)
        tree
        (let [mid (quot (+ from to) 2)]
          (assoc tree
            :left (create from mid)
            :right (create (inc mid) to))))))

(defn query-max [tree from to]
  (let [t-from (:from tree)
        t-to (:to tree)]
    (cond (<= from t-from t-to to) (:max tree)
          (< t-to from) 0
          (< to t-from) 0
          :else (max (query-max (:left tree) from to)
                     (query-max (:right tree) from to)))))

(defn- inside? [tree ind]
  (<= (:from tree) ind (:to tree)))

(defn update [{:keys [left right from to] :as tree} ind value]
  (if (= from ind to)
    (assoc tree :max value)
    (let [left (if (inside? left ind)
                 (update left ind value)
                 left)
          right (if (inside? right ind)
                  (update right ind value)
                  right)]
      (assoc tree
        :left left
        :right right
        :max (max (:max left) (:max right))))))


#_(let [t (create 0 100)]
  (assert (= 0 (query-max t 0 100)))
  (assert (= 0 (query-max t 2 40)))
  (let [t (update t 42 8)]
    (assert (= 8 (query-max t 0 100)))
    (assert (= 8 (query-max t 10 42)))
    (assert (= 8 (query-max t 30 50)))
    (assert (= 0 (query-max t 0 40)))
    (assert (= 0 (query-max t 50 60)))))
