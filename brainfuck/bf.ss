(import (chezscheme))

(define-record-type op (fields op val))
(define-record-type tape (fields data pos))

;;; Printer.

(define-record-type printer (fields (mutable sum1) (mutable sum2) quiet))

(define (print p n)
  (if (printer-quiet p)
      (begin
          (printer-sum1-set! p (remainder
                                (+ (printer-sum1 p) n)
                                255))
          (printer-sum2-set! p (remainder
                                (+ (printer-sum2 p) (printer-sum1 p))
                                255)))
      (begin
          (display (integer->char n))
          (flush-output-port))))

(define (get-checksum p)
  (bitwise-ior (ash (printer-sum2 p) 8) (printer-sum1 p)))

;;; Vector and tape ops.

(define (vector-copy! dest dest-start src)
  (define i dest-start)
  (vector-for-each
    (lambda (x)
      (vector-set! dest i x)
      (set! i (+ 1 i)))
    src))

(define (vector-grow-if-needed vec len)
  (if (<= len (vector-length vec))
      vec
      (let ((new-vec (make-vector len)))
        (vector-copy! new-vec 0 vec)
        new-vec)))

(define (tape-get t)
  (vector-ref (tape-data t) (tape-pos t)))

(define (tape-move t n)
  (let ((new-pos (+ n (tape-pos t))))
    (make-tape
      (vector-grow-if-needed (tape-data t) (add1 new-pos))
      new-pos)))

(define (tape-inc! t n)
  (let ((data (tape-data t)) (pos (tape-pos t)))
    (vector-set! data pos (+ n (vector-ref data pos)))
    t))

;;; Parser.

(define (parse-helper lst acc)
  (if (null? lst)
      (reverse acc)
      (let ((rst (cdr lst)))
        (case (car lst)
          ((#\+) (parse-helper rst (cons (make-op 'inc 1) acc)))
          ((#\-) (parse-helper rst (cons (make-op 'inc -1) acc)))
          ((#\>) (parse-helper rst (cons (make-op 'move 1) acc)))
          ((#\<) (parse-helper rst (cons (make-op 'move -1) acc)))
          ((#\.) (parse-helper rst (cons (make-op 'print '()) acc)))
          ((#\[) (let ((subparsed (parse-helper rst '())))
                      (parse-helper
                        (car subparsed)
                        (cons
                          (make-op 'loop (cdr subparsed))
                          acc))))
          ((#\]) (cons rst (reverse acc)))
          (else (parse-helper rst acc))))))

(define (parse bf-code) (parse-helper (string->list bf-code) '()))

;;; Interpreter.

(define (run parsed t p)
  (if (null? parsed)
    t
    (let* ((op (op-op (car parsed)))
           (val (op-val (car parsed)))
           (rst (cdr parsed)))
      (case op
        ((inc) (run rst (tape-inc! t val) p))
        ((move) (run rst (tape-move t val) p))
        ((print)
         (print p (tape-get t))
         (run rst t p))
        ((loop)
         (if (> (tape-get t) 0)
             (run parsed (run val t p) p)
             (run rst t p)))
        (else (run rst t p))))))

;;; I/O.
(load-shared-object "../common/libnotify/target/libnotify.so")

(define notify-internal
  (foreign-procedure "notify" (u8* size_t) void))

(define (notify msg)
  (let ([bv (string->utf8 msg)])
     (notify-internal bv (bytevector-length bv))))

(define (get-file-arg-or-exit)
  (let ((cl (command-line)))
    (cond
      ((= (length cl) 2) (list-ref cl 1))
      (else
        (printf "usage: ~a filename" (list-ref (command-line) 0))
        (exit)))))

(define (file->string path)
  (call-with-input-file path
    (lambda (port) (get-string-all port))))

(define (verify)
  (define text "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  (define p-left (make-printer 0 0 #t))
  (define p-right (make-printer 0 0 #t))

  (run (parse text) (make-tape (make-vector 1) 0) p-left)
  (for-each
   (lambda (c) (print p-right (char->integer c)))
   (string->list "Hello World!\n"))

  (let ((left (get-checksum p-left))
        (right (get-checksum p-right)))
    (if (not (eq? left right))
        (errorf 'verify "~s != ~s" left right))))

(define (main)
  (define text (file->string (get-file-arg-or-exit)))
  (define p (make-printer 0 0 (getenv "QUIET")))

  (notify (format "Chez Scheme\t~s" (get-process-id)))
  (run (parse text) (make-tape (make-vector 1) 0) p)
  (notify "stop")

  (if (printer-quiet p) (printf "Output checksum: ~s\n" (get-checksum p))))

((lambda ()
   (verify)
   (main)
))
