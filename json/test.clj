(ns json
  (:require [cheshire.core :refer [parse-string]]
            [clojure.java.io :refer [reader]]))

(defn parse [text]
  (time
   (let [data (:coordinates (parse-string text true))
         len  (count data)]
     (loop [sx 0.0 sy 0.0 sz 0.0 [coord & coords] data]
       (if-let [{:keys [x y z]} coord]
         (recur (+ sx x) (+ sy y) (+ sz z) coords)
         (println (/ sx len) (/ sy len) (/ sz len)))))))

(defn notify [msg]
  (try
    (with-open [sock (java.net.Socket. "localhost" 9001)
                printer (java.io.PrintWriter. (.getOutputStream sock))]
      (.println printer msg))
    (catch java.io.IOException e ())))

(let [text (slurp "/tmp/1.json")]
  (dotimes [i 4] (parse text))

  (notify (format "Clojure\t%d" (.pid (java.lang.ProcessHandle/current))))
  (parse text)
  (notify "stop"))
