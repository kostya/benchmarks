#lang racket
(require racket/file racket/list racket/match racket/cmdline
         (rename-in racket/unsafe/ops
                    [unsafe-vector-ref vector-ref]
                    [unsafe-vector-set! vector-set!]
                    [unsafe-fx+ +]))

(define-syntax-rule (define-match-expander* k r) (define-match-expander k r r))
(define-match-expander* op (syntax-rules () [(_ op val) (cons op val)]))
(define-match-expander* tape (syntax-rules () [(_ data pos) (mcons data pos)]))

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
  (match-let ([(tape data pos) t])
    (vector-ref data pos)))

(define (tape-move! t n)
  (match-let ([(tape data pos) t])
    (let ([new-pos (+ n pos)])
      (set-mcar! t (vector-grow-if-needed data new-pos))
      (set-mcdr! t new-pos))))

(define (tape-inc! t n)
  (match-let ([(tape data pos) t])
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

(define (run parsed t)
  (define step-op!
    (match-lambda
      [(op 'inc x) (tape-inc! t x)]
      [(op 'move x) (tape-move! t x)]
      ['print (display (integer->char (tape-get t)))
              (flush-output)]
      [(op 'loop body) (let loop ()
                         (when (> (tape-get t) 0)
                           (step-ops! body)
                           (loop)))]))
  (define step-ops!
    (match-lambda
      [(cons op ops) (step-op! op) (step-ops! ops)]
      [_ (void)]))
  (step-ops! parsed))

;;; I/O.
(with-handlers ([exn:fail:network? (lambda (_) (void))])
  (let-values ([(in out) (tcp-connect "localhost" 9001)])
    (display "Racket" out)
      (close-output-port out)))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->string path)))
(run (parse (read-c (command-line #:args (filename) filename))) (tape (vector 0) 0))
