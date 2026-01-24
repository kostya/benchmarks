(def OP_INC 0)
(def OP_MOVE 1)
(def OP_LOOP 2)
(def OP_PRINT 3)

(defn read-file [file-name]
  (with [f (file/open file-name)]
        (map string/from-bytes (file/read f :all))))

(defn parse-text [code & [pos]]
  (var res @[])
  (var idx (or pos 0))
  (def len (length code))
  (label result
         (while (< idx len)
           (let [c (get code idx)]
             (++ idx)
             (case c
               "+" (array/push res [OP_INC 1])
               "-" (array/push res [OP_INC -1])
               ">" (array/push res [OP_MOVE 1])
               "<" (array/push res [OP_MOVE -1])
               "." (array/push res [OP_PRINT 0])
               "[" (let [[sub-res sub-idx] (parse-text code idx)]
                     (array/push res [OP_LOOP sub-res])
                     (set idx sub-idx))
               "]" (return result [res idx])
               nil))))
  [res idx])

(defn make-tape []
  @{:tape @[0] :pos 0})

(defn tape-get [tape]
  (get (tape :tape) (tape :pos)))

(defn tape-inc [tape x]
  (let [t (tape :tape)
        p (tape :pos)]
    (+= (t p) x)))

(defn tape-move [tape x]
  (let [t (tape :tape)
        len (length t)]
    (+= (tape :pos) x)
    (when (>= (tape :pos) len)
      (array/join t (array/new-filled (* 2 len) 0)))))

(defn make-printer [quiet]
  @{:sum1 0 :sum2 0 :quiet quiet})

(defn tape-print [printer n]
  (if (printer :quiet)
    (do
      (set (printer :sum1)
           (% (+ (printer :sum1) n) 255))
      (set (printer :sum2)
           (% (+ (printer :sum1) (printer :sum2)) 255)))
    (do
      (file/write stdout (string/from-bytes n))
      (file/flush stdout))))

(defn tape-checksum [printer]
  (let [sum1 (printer :sum1)
        sum2 (printer :sum2)]
    (bor (blshift sum2 8) sum1)))

(defn run [ops tape printer]
  (each [op val] ops
    (case op
      OP_INC (tape-inc tape val)
      OP_MOVE (tape-move tape val)
      OP_LOOP (while (> (tape-get tape) 0)
                (run val tape printer))
      OP_PRINT (tape-print printer (tape-get tape))
      nil)))

(defn notify [msg]
  (try
    (with [s (net/connect "localhost" 9001)]
          (net/write s msg))
    ([err] (eprint "Connection failed: " err))))

(defn verify []
  (def text "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
             ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

  (def p-left (make-printer true))
  (var tape (make-tape))
  (run (first (parse-text (map string/from-bytes text))) tape p-left)
  (def left (tape-checksum p-left))

  (def p-right (make-printer true))
  (each c "Hello World!\n"
    (tape-print p-right c))
  (def right (tape-checksum p-right))
  (when (not= left right)
    (eprintf "%d != %d" left right)
    (os/exit 1)))

(verify)
(def text (first (parse-text (read-file (get (dyn :args) 1)))))
(def printer (make-printer (not (empty? (or (os/getenv "QUIET") "")))))

(notify (string/format "Janet\t%d" (os/getpid)))
(run text (make-tape) printer)
(notify "stop")

(when (printer :quiet)
  (print "Output checksum: " (tape-checksum printer)))
