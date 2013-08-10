(ns wordament
  (:require [clojure.java.io :as io]
            [clojure.string :refer (lower-case split)]
            [clojure.set :refer (union)])
  (:gen-class))

(defn add-word
  "Добавляет слово в дерево Бора и возвращает новое дерево."
  [tree [fst & rst]]
  (if (nil? fst)
    (assoc tree :word? true)
    (let [subtree (tree fst {})]
      (assoc tree fst (add-word subtree rst)))))

(defn load-dictionary
  "Загружает словарь из zip файла, а так же удаляет все слова c < 3 и > 16 буквами.
  Возвращает список слов."
  []
  (let [zip (-> (io/resource "dict.zip")
                (.openStream)
                (java.util.zip.ZipInputStream.))]
    (.getNextEntry zip)
    (->> zip
         io/reader
         line-seq
         (map lower-case)
         (filter #(< 2 (count %) 17))
         (filter #(neg? (.indexOf % "-"))))))

(def cost-string "А:1,Б:5,В:2,Г:4,Д:4,Е:1,Ё:0,Ж:6,З:4,И:1,Й:5,К:3,Л:2,М:2,Н:1,О:1,П:3,Р:2,С:2,Т:2,У:3,Ф:7,Х:5,Ц:7,Ч:5,Ш:4,Щ:0,Ъ:0,Ы:4,Ь:5,Э:10,Ю:4,Я:3")

; Мапка со ценами букв.
(def costs (->> (split cost-string #",")
                (map lower-case)
                (map (fn [[letter _ digit]]
                       [letter (- (int digit) (int \0))]))
                (into {})))

(defn word-price [word]
  (reduce + (map costs word)))

(defn neibs
  "Строит список всех возможных соседей клетки (x, y)."
  [x y]
  (for [nx (range (dec x) (+ x 2))
        ny (range (dec y) (+ y 2))
        :when (and (<= 0 nx 3)
                   (<= 0 ny 3)
                   (not (and (= nx x)
                             (= ny y))))]
    [nx ny]))

(defn build-field
  "Строит вместо поля букв мапку, в которой ключ - координаты буквы, а значениe - мапка с самой буквой и соседями.
  Таким образом можно быстро получить всех соседей конкретной клетки, без необходимости использовать neibs."
  [table]
  (let [table (map lower-case table)]
    (->> (for [i (range 4)
               j (range 4)]
           [[i j] {:letter (nth (nth table i) j)
                   :neibs (neibs i j)}])
         (into {}))))

; В этой переменной будет находится поле.
(def ^:dynamic *field*)

(defn dfs
  "Поиск слов через поиск в глубину.
  cur - координаты текущей клетки
  visited - множество посещённых координат
  tree - текущее поддерево Бора в котором мы находимся
  accum - текущие накопленные буквы
  Возвращает множество найденных слов."
  [cur visited tree accum]
  (let [res (if (:word? tree) #{accum} #{})
        possible-moves (->> (*field* cur)
                            :neibs
                            (remove visited))
        try-neib (fn [neib]
                   (let [value (:letter (*field* neib))]
                     (if-let [subtree (tree value)]
                       (dfs neib (conj visited neib) subtree (str accum value))
                       [])))]
    (->> (map try-neib possible-moves)
         (reduce union res)
         doall)))

(defn find-words
  "Ищет и печатает слова из таблицы, отсортированные по стоимости."
  [table]
  (binding [*field* (build-field table)]
    (let [_ (println "Построение дерева Бора по словарю")
          tree (time (reduce add-word {} (load-dictionary)))
          _ (println "Поиск слов")
          words (time (->> (for [[pos {:keys [letter]}] *field*
                                 :when (tree letter)]
                             (dfs pos #{letter} (tree letter) (str letter)))
                           (reduce union)
                           (sort-by #(- (word-price %)))))]
      (doseq [word words]
        (println word (word-price word)))
      (println "Всего:" (count words)))))

(defn -main [& args]
  (if (empty? args)
    "Программе должен был быть передан файл с полем 4x4."
    (->> (first args)
         io/file
         io/reader
         line-seq
         find-words)))
