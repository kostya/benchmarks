(ns json
  (:require [cheshire.core :refer [parse-string]]
            [clojure.java.io :refer [reader]]))

(defrecord Coordinate [x y z])

(defn calc [text]
  (let [data (:coordinates (parse-string text true))
        len (count data)]
    (loop [sx 0.0 sy 0.0 sz 0.0 [coord & coords] data]
      (if-let [{:keys [x y z]} coord]
        (recur (+ sx x) (+ sy y) (+ sz z) coords)
        (Coordinate. (/ sx len) (/ sy len) (/ sz len))))))

(defn notify [msg]
  (try
    (with-open [sock (java.net.Socket. "localhost" 9001)
                printer (java.io.PrintWriter. (.getOutputStream sock))]
      (.println printer msg))
    (catch java.io.IOException e ())))

(defn verify [[right v]]
  (let [left (calc v)]
    (if (not= left right)
      (do
        (binding [*out* *err*] (println left "!=" right))
        (System/exit 1))
      ())
    ))

(let [right (Coordinate. 2.0 0.5 0.25)
      lefts '("{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"
              "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}")
      verification_pairs (map (fn [x] (list right x)) lefts)]
  (do
    (run! verify verification_pairs)
    (let [text (slurp "/tmp/1.json")]
      (notify (format "Clojure\t%d" (.pid (java.lang.ProcessHandle/current))))
      (let [results (calc text)]
        (do
          (notify "stop")
          (println results))))))
