(require '[cheshire.core :refer :all])

(let [data (:coordinates (parse-stream (clojure.java.io/reader "./1.json") true))
      len  (count data)]
  (loop [sx 0.0 sy 0.0 sz 0.0 [coord & coords] data]
    (if-let [{:keys [x y z]} coord]
      (recur (+ sx x) (+ sy y) (+ sz z) coords)
      (println (/ sx len) (/ sy len) (/ sz len)))))
