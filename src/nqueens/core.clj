(ns nqueens.core)

(def default-size 8)

(defn print-board [queen-locations]
  (let [size (count queen-locations)
        unfilled (vec (repeat size "-"))]
    (doseq [queen-location queen-locations]
      (let [filled (if (not= queen-location -1)
                     (assoc unfilled queen-location "Q")
                     unfilled)]
        (apply println filled)))
    (newline)))

(defn valid-move? [queens new-queen]
  (let [new-column (count queens)]
    (not-any? true?
              (for [ith-column (range new-column)
                    :let [ith-queen (queens ith-column)]]
                (or (= new-queen ith-queen)
                    (= (- new-column ith-column)
                       (Math/abs (- new-queen ith-queen))))))))

(defn add-valid-moves [boards size]
  (for [board boards
        new-queen (range 0 size) :when (valid-move? board new-queen)]
    (conj board new-queen)))

(defn nqueens [size]
  (nth (iterate #(add-valid-moves % size) [[]]) size))

(defn -main [& args]
  (let [valid-boards (nqueens default-size)]
    (doseq [valid-board valid-boards] (print-board valid-board))
    (println (count valid-boards) "valid boards of size" default-size "!")))
