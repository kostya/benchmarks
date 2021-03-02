#lang racket/base
(require racket/file racket/tcp racket/cmdline racket/os racket/fixnum
         (rename-in racket/unsafe/ops
                    [unsafe-vector*-ref vector-ref]
                    [unsafe-vector*-set! vector-set!]
                    [unsafe-vector*-length vector-length]
                    [unsafe-fx+ +]))
(#%declare #:unsafe)
(struct op (op val) #:authentic)
(struct tape (data pos) #:authentic)

;;; Printer.

(struct printer ([sum1 #:mutable] [sum2 #:mutable] quiet) #:authentic)

(define (print p n)
  (if (printer-quiet p)
      (begin
          (set-printer-sum1! p (fxremainder
                                (+ (printer-sum1 p) n)
                                255))
          (set-printer-sum2! p (fxremainder
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
  (cond [(fx< len old-len) vec]
        [else
         (let loop ([new-len (fx* 2 old-len)])
           (cond [(fx>= len new-len) (loop (fx* 2 new-len))]
                 [else (define new-vec (make-vector new-len))
                       (vector-copy! new-vec 0 vec)
                       new-vec]))]))

(define (tape-get t)
  (vector-ref (tape-data t) (tape-pos t)))

(define (tape-move t n)
  (let ([new-pos (+ n (tape-pos t))])
    (tape (vector-grow-if-needed (tape-data t) new-pos) new-pos)))

(define (tape-inc! t n)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (vector-set! data pos (+ n (vector-ref data pos)))))

;;; Parser.

(define (parse-helper lst acc)
  (if (null? lst)
      (reverse acc)
      (let ([rst (cdr lst)])
        (case (car lst)
          [(#\+) (parse-helper rst (cons (op 'inc 1) acc))]
          [(#\-) (parse-helper rst (cons (op 'inc -1) acc))]
          [(#\>) (parse-helper rst (cons (op 'move 1) acc))]
          [(#\<) (parse-helper rst (cons (op 'move -1) acc))]
          [(#\.) (parse-helper rst (cons (op 'print -1) acc))]
          [(#\[) (let ([subparsed (parse-helper rst '())])
                 (parse-helper (car subparsed)
                               (cons (op 'loop (cdr subparsed)) acc)))]
          [(#\]) (cons rst (reverse acc))]
          [else (parse-helper rst acc)]))))

(define (parse bf-code) (parse-helper (string->list bf-code) '()))

;;; Interpreter.

(define (run parsed t p)
  (let loop ([parsed parsed] [t t])
    (cond
      [(null? parsed) t]
      [else
       (define fst (car parsed))
       (define op (op-op fst))
       (define val (op-val fst))
       (define rst (cdr parsed))
       (case op
         [(inc) (tape-inc! t val)
                (loop rst t)]
         [(move) (loop rst (tape-move t val))]
         [(print) (print p (tape-get t))
                  (loop rst t)]
         [(loop)
          (if (> (tape-get t) 0)
              (loop parsed (run val t p))
              (loop rst t))]
         [else (loop rst t)])])))

(define (notify msg)
  (with-handlers ([exn:fail:network? (lambda (_) (void))])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->string path)))

(define (verify)
  (define text "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  (define p-left (printer 0 0 #t))
  (define p-right (printer 0 0 #t))

  (run (parse text) (tape (vector 0) 0) p-left)
  (for-each
   (lambda (c) (print p-right (char->integer c)))
   (string->list "Hello World!\n"))

  (let ((left (get-checksum p-left))
        (right (get-checksum p-right)))
    (when (not (eq? left right))
        (error 'verify "~s != ~s" left right))))

(module+ main
  (verify)
  (define text (read-c (command-line #:args (filename) filename)))
  (define p (printer 0 0 (getenv "QUIET")))

  (notify (format "Racket\t~s" (getpid)))
  (void (run (parse text) (tape (vector 0) 0) p))
  (notify "stop")

  (when (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))
