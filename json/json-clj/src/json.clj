(ns json
  (:require [cheshire.core :refer [parse-stream]]
            [clojure.java.io :refer [reader]])
  (:gen-class))

(defn -main []
  (time
    (let [data (:coordinates (parse-stream (reader "./1.json") true))
          len  (count data)]
      (loop [sx 0.0 sy 0.0 sz 0.0 [coord & coords] data]
        (if-let [{:keys [x y z]} coord]
          (recur (+ sx x) (+ sy y) (+ sz z) coords)
          (println (/ sx len) (/ sy len) (/ sz len)))))))
