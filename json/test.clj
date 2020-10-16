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

(defn parse [text]
  (time (println (calc text))))

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

(let [right (Coordinate. 1.1 2.2 3.3)
      lefts '("{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}"
              "{\"coordinates\":[{\"y\":2.2,\"x\":1.1,\"z\":3.3}]}")
      verification_pairs (map (fn [x] (list right x)) lefts)]
  (do
    (run! verify verification_pairs)
    (let [text (slurp "/tmp/1.json")]
      (notify (format "Clojure\t%d" (.pid (java.lang.ProcessHandle/current))))
      (parse text)
      (notify "stop"))))
