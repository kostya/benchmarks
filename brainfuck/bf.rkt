#lang racket
(require racket/file racket/list racket/match racket/cmdline racket/os
         (rename-in racket/unsafe/ops
                    [unsafe-vector-ref vector-ref]
                    [unsafe-vector-set! vector-set!]
                    [unsafe-fx+ +]))

(struct op (op val))
(struct tape ([data #:mutable] [pos #:mutable]))

;;; Printer.

(struct printer ([sum1 #:mutable] [sum2 #:mutable] quiet))

(define (print p n)
  (if (printer-quiet p)
      (begin
          (set-printer-sum1! p (remainder
                                (+ (printer-sum1 p) n)
                                255))
          (set-printer-sum2! p (remainder
                                (+ (printer-sum2 p) (printer-sum1 p))
                                255)))
      (begin
          (display (integer->char n))
          (flush-output))))

(define (get-checksum p)
  (bitwise-ior (arithmetic-shift (printer-sum2 p) 8) (printer-sum1 p)))

;;; Vector and tape ops.

(define (vector-grow-if-needed vec len)
  (define old-len (vector-length vec))
  (cond [(< len old-len) vec]
        [else
         (let loop ([new-len (* 2 old-len)])
           (cond [(>= len new-len) (loop (* 2 new-len))]
                 [else (define new-vec (make-vector new-len))
                       (vector-copy! new-vec 0 vec)
                       new-vec]))]))

(define (tape-get t)
  (vector-ref (tape-data t) (tape-pos t)))

(define (tape-move! t n)
  (let ([new-pos (+ n (tape-pos t))])
    (set-tape-data! t (vector-grow-if-needed (tape-data t) new-pos))
    (set-tape-pos! t new-pos)))

(define (tape-inc! t n)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (vector-set! data pos (+ n (vector-ref data pos)))))

;;; Parser.

(define (parse-helper lst acc)
  (if (empty? lst)
      (reverse acc)
      (let ([rst (rest lst)])
        (match (first lst)
          [#\+ (parse-helper rst (cons (op 'inc 1) acc))]
          [#\- (parse-helper rst (cons (op 'inc -1) acc))]
          [#\> (parse-helper rst (cons (op 'move 1) acc))]
          [#\< (parse-helper rst (cons (op 'move -1) acc))]
          [#\. (parse-helper rst (cons 'print acc))]
          [#\[ (let ([subparsed (parse-helper rst empty)])
                 (parse-helper (first subparsed)
                               (cons (op 'loop (rest subparsed)) acc)))]
          [#\] (cons rst (reverse acc))]
          [_ (parse-helper rst acc)]))))

(define (parse bf-code) (parse-helper (string->list bf-code) empty))

;;; Interpreter.

(define (run parsed t p)
  (define step-op!
    (match-lambda
      [(op 'inc x) (tape-inc! t x)]
      [(op 'move x) (tape-move! t x)]
      ['print (print p (tape-get t))]
      [(op 'loop body) (let loop ()
                         (when (> (tape-get t) 0)
                           (step-ops! body)
                           (loop)))]))
  (define step-ops!
    (match-lambda
      [(cons op ops) (step-op! op) (step-ops! ops)]
      [_ (void)]))
  (step-ops! parsed))

(define (notify msg)
  (with-handlers ([exn:fail:network? (lambda (_) (void))])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
        (close-output-port out))))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->string path)))

(module+ main
  (define text null)
  (define p (printer 0 0 (getenv "QUIET")))
  (set! text (read-c (command-line #:args (filename) filename)))

  (notify (format "Racket\t~s" (getpid)))
  (run (parse text) (tape (vector 0) 0) p)
  (notify "stop")

  (when (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))
