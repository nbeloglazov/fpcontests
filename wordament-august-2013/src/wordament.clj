(ns wordament
  (:require [clojure.java.io :as io]))

(defn add-word [tree [fst & rst]]
  (if (nil? fst)
    (assoc tree :word? true)
    (let [subtree (tree fst {})]
      (assoc tree fst (add-word subtree rst)))))

(def dict (let [zip (-> (io/resource "dict.zip")
                        (.openStream)
                        (java.util.zip.ZipInputStream.))]
            (.getNextEntry zip)
            (->> zip
                 io/reader
                 line-seq
                 (filter #(< 2 (count %) 17))
                 (filter #(neg? (.indexOf % "-"))))))

(def tree (reduce add-word {} dict))
