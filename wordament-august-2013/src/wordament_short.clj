(ns wordament-short
  (:require [clojure.java.io :as io]
            [clojure.string :refer (upper-case split)]
            [clojure.set :refer (union)])
  (:gen-class))

(defn add-word [tree [fst & rst]]
  (if (nil? fst)
    (assoc tree :word? true)
    (let [subtree (tree fst {})]
      (assoc tree fst (add-word subtree rst)))))

(defn load-dictionary []
  (let [zip (-> (io/resource "dict.zip") .openStream java.util.zip.ZipInputStream.)]
    (.getNextEntry zip)
    (->> zip io/reader line-seq (map upper-case)
         (filter #(< 2 (count %) 17))
         (filter #(neg? (.indexOf % "-"))))))

(def costs {\А 1 \Б 5 \В 2 \Г 4 \Д 4 \Е 1 \Ё 0 \Ж 6 \З 4 \И 1 \Й 5 \К 3 \Л 2 \М 2 \Н 1 \О 1 \П 3
            \Р 2 \С 2 \Т 2 \У 3 \Ф 7 \Х 5 \Ц 7 \Ч 5 \Ш 4 \Щ 0 \Ъ 0 \Ы 4 \Ь 5 \Э 10 \Ю 4 \Я 3})

(defn word-price [word]
  (reduce + (map costs word)))

(defn neibs [[x y]]
  (for [nx (range (- x 1) (+ x 2))
        ny (range (- y 1) (+ y 2))
        :when (and (<= 0 nx 3) (<= 0 ny 3))]
    [nx ny]))

(defn dfs [cur visited tree accum field]
  (if (nil? tree) []
      (let [res (if (:word? tree) #{accum} #{})
            try-neib #(let [value (get-in field %)]
                        (dfs % (conj visited %) (tree value) (str accum value) field))]
        (->> (neibs cur) (remove visited) (map try-neib) (reduce union res)))))

(defn find-words [field]
  (let [tree (time (reduce add-word {} (load-dictionary)))
        words (time (->> (for [i [0 1 2 3] j [0 1 2 3]
                               :let [letter (get-in field [i j])]]
                           (dfs [i j] #{letter} (tree letter) (str letter) field))
                         (reduce union)
                         (sort-by word-price)))]
    (doseq [word words]
      (println word (word-price word)))))

(defn -main [& args]
  (if (empty? args)
    "Программе должен был быть передан файл с полем 4x4."
    (->> (first args) io/file io/reader line-seq (map upper-case) (mapv vec) find-words)))
