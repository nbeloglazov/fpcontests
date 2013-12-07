(ns car-parking.core
  (:require [clojure.java.io :as io]))

(def delta {:up [-1 0]
            :down [1 0]
            :left [0 -1]
            :right [0 1]})

(def types-head-first
  #{[:vertical :up]
    [:horizontal :left]})

(def types-head-last
  #{[:vertical :down]
    [:horizontal :right]})

(defn next-cell [{:keys [body type]} dir]
  (condp contains? [type dir]
     types-head-first (map + (first body) (delta dir))
     types-head-last (map + (last body) (delta dir))
     nil))

(defn move-car [{:keys [body type] :as car} dir]
  (let [new-cell (next-cell car dir)
        [new-body tail-cell] (condp contains? [type dir]
                               types-head-first [(cons new-cell (butlast body)) (last body)]
                               types-head-last [(concat (rest body) [new-cell]) (first body)])]
    [(assoc car :body new-body) tail-cell]))

(defn can-move-car? [car dir empty-cells]
  (contains? empty-cells (next-cell car dir)))

(defn update-state [state car-id dir]
  (let [car (get-in state [:cars car-id])
        new-cell (next-cell car dir)
        [new-car emptied-cell] (move-car car dir)]
    (-> state
        (assoc-in [:cars car-id] new-car)
        (update-in [:empty-cells] conj emptied-cell)
        (update-in [:empty-cells] disj new-cell))))

(defn available-moves [{:keys [cars empty-cells]}]
  (for [[id car] cars
        dir [:up :down :left :right]
        :when (can-move-car? car dir empty-cells)]
    [id dir]))

(defn new-states [state moves covered]
  (->> moves
       (map #(apply update-state state %))
       (remove #(contains? covered (:empty-cells %)))))

(defn update-covered [cur-state new-states covered]
  (reduce #(assoc %1 (:empty-cells %2) cur-state) covered new-states))

(defn final-state? [{:keys [cars final-cell]}]
  (= (last (get-in cars [:# :body])) final-cell))

(defn build-solution [final-state covered]
  (loop [solution []
         cur-state final-state]
    (if (nil? cur-state)
      solution
      (recur (cons cur-state solution)
             (covered (:empty-cells cur-state))))))

(defn solve [initial-state]
  (loop [queue [initial-state]
         covered {(:empty-cells initial-state) nil}]
    (if (empty? queue)
      nil
      (let [[cur & rst] queue]
        (if (final-state? cur)
          (build-solution cur covered)
          (let [new-st (new-states cur (available-moves cur) covered)]
            (recur (concat rst new-st)
                   (update-covered cur new-st covered ))))))))

(defn print-state [{:keys [empty-cells cars]}]
  (let [cells (apply merge
                     (zipmap empty-cells (repeat \.))
                     (for [[id car] cars]
                       (zipmap (:body car) (repeat (name id)))))
        width (->> (keys cells)
                   (map second)
                   (apply max))
        height (->> (keys cells)
                    (map first)
                    (apply max))]
    (dotimes [y (inc height)]
      (dotimes [x (inc width)]
        (print (cells [y x])))
      (println))))

(defn print-solution [solution]
  (doseq [state solution]
    (print-state state)
    (println))
  (println "Total steps" (count solution)))

(defn build-car [cells]
  (let [left (->> (map second cells) (apply min))
        right (->> (map second cells) (apply max))]
    (if (= left right)
      {:type :vertical
       :body (sort-by first cells)}
      {:type :horizontal
       :body (sort-by second cells)})))

(defn read-input [file]
  (let [lines (line-seq (io/reader file))
        cells-by-char (->> (for [y (range (count lines))
                                 :let [line (nth lines y)]
                                 x (range (count line))]
                             {(nth line x) [[y x]]})
                           (apply merge-with concat))
        cars (into {}
               (for [[char cells] cells-by-char
                     :when (not= char \.)]
                 [(keyword (str char)) (build-car cells)]))
        goal-car-y (-> cars :# :body first first)
        final-cell [goal-car-y (dec (count (first lines)))]]
    {:cars cars
     :empty-cells (set (cells-by-char \.))
     :final-cell final-cell}))

(defn -main [& args]
  (if-let [input (first args)]
    (->> input
         read-input
         solve
         print-solution)
    (println "Provide input file as first argument.")))
