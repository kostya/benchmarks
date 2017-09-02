#lang racket/base
(require racket/file racket/list racket/match)

(struct op (op val) #:transparent)
(struct tape (data pos) #:transparent)

;;; Vector and tape ops.

(define (vector-grow-if-needed vec len)
  (if (<= len (vector-length vec))
      vec
      (let ((new-vec (make-vector len)))
        (vector-copy! new-vec 0 vec)
        new-vec)))

(define (tape-get t)
  (match t ((tape data pos) (vector-ref data pos))))

(define (tape-move t n)
  (match t ((tape data pos)
            (let ((new-pos (+ n pos)))
              (tape (vector-grow-if-needed data (add1 new-pos)) new-pos)))))

(define (tape-inc! t n)
  (match t ((tape data pos)
            (vector-set! data pos (+ n (vector-ref data pos)))
            t)))

;;; Parser.

(define (parse-helper lst acc)
  (if (empty? lst)
      (reverse acc)
      (let ((rst (rest lst)))
        (match (first lst)
          (#\+ (parse-helper rst (cons (op 'inc 1) acc)))
          (#\- (parse-helper rst (cons (op 'inc -1) acc)))
          (#\> (parse-helper rst (cons (op 'move 1) acc)))
          (#\< (parse-helper rst (cons (op 'move -1) acc)))
          (#\. (parse-helper rst (cons (op 'print null) acc)))
          (#\[ (let ((subparsed (parse-helper rst empty)))
                 (parse-helper (first subparsed)
                               (cons (op 'loop (rest subparsed)) acc))))
          (#\] (cons rst (reverse acc)))
          (_ (parse-helper rst acc))))))

(define (parse bf-code) (parse-helper (string->list bf-code) empty))

;;; Interpreter.

(define (run parsed t)
  (match parsed
    ((list) t)
    ((list-rest (op 'inc x) rst) (run rst (tape-inc! t x)))
    ((list-rest (op 'move x) rst) (run rst (tape-move t x)))
    ((list-rest (op 'print _) rst)
     (display (integer->char (tape-get t)))
     (flush-output)
     (run rst t))
    ((list-rest (op 'loop body) rst)
     (if (> (tape-get t) 0)
         (run parsed (run body t))
         (run rst t)))
    ((list-rest _ rst) (run rst t))))

;;; I/O.

(define (get-file-arg-or-exit)
  (match (current-command-line-arguments)
    ((vector path) path)
    (_ ((printf "usage: ~a filename" (find-system-path 'run-file))
        (exit)))))

(define (read-c path)
  (parameterize ((current-locale "C"))
    (file->string path)))

(run (parse (read-c (get-file-arg-or-exit))) (tape (vector 0) 0))
