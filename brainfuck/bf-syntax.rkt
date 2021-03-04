#lang racket/base

(require racket/file racket/list racket/match racket/cmdline racket/os racket/tcp
         (rename-in racket/unsafe/ops
                    [unsafe-vector*-ref vector-ref]
                    [unsafe-vector*-set! vector-set!]
                    [unsafe-vector*-length vector-length]
                    [unsafe-fx+ +]
                    [unsafe-fxremainder remainder]
                    [unsafe-fx* *]
                    [unsafe-fx< <]
                    [unsafe-fx>= >=]))

(#%declare #:unsafe)
(struct tape (data pos))

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

;;; Parser.
(define (parse bf-code)
  (define in (open-input-string bf-code))
  (let go ([e #'pos])
    (match (read-char in)
      [#\+ (go #`(tape-inc! #,e  1))]
      [#\- (go #`(tape-inc! #,e -1))]
      [#\> (go #`(tape-move #,e  1))]
      [#\< (go #`(tape-move #,e -1))]
      [#\. (go #`(tape-print #,e))]
      [#\[ (let ([body (go #'pos)]
                 [end  (go #'pos)])
             #`(let loop ([pos #,e])
                 (if (> (tape-get pos) 0) (loop #,body) #,end)))]
      [#\] e]
      [(? eof-object?) e]
      [_ (go e)])))

;;; Interpreter.
(define (run parsed t p)
  ((eval #`(λ (data pos p)
             (define (tape-get pos) (vector-ref data pos))
             (define (tape-move pos n)
               (define pos* (+ n pos))
               (vector-grow-if-needed! pos*)
               pos*)
             (define (tape-inc! pos n)
               (vector-set! data pos (+ n (vector-ref data pos)))
               pos)
             (define (tape-print pos) (print p (tape-get pos)) pos)
             (define (vector-grow-if-needed! len)
               (define old-len (vector-length data))
               (when (>= len old-len)
                 (let loop ([new-len (* 2 old-len)])
                   (cond [(>= len new-len) (loop (* 2 new-len))]
                         [else (define new-vec (make-vector new-len))
                               (vector-copy! new-vec 0 data)
                               (set! data new-vec)]))))
             (void #,parsed)))
   (tape-data t) (tape-pos t) p))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
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

  (notify (format "Racket (Syntax Objects)\t~s" (getpid)))
  (run (parse text) (tape (vector 0) 0) p)
  (notify "stop")

  (when (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))
