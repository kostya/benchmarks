(define-record-type op (fields op val))
(define-record-type tape (fields data pos))

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

(define (run parsed t)
  (if (null? parsed)
    t
    (let* ((op (op-op (car parsed)))
           (val (op-val (car parsed)))
           (rst (cdr parsed)))
      (case op
        ((inc) (run rst (tape-inc! t val)))
        ((move) (run rst (tape-move t val)))
        ((print)
         (display (integer->char (tape-get t)))
         (flush-output-port)
         (run rst t))
        ((loop)
         (if (> (tape-get t) 0)
             (run parsed (run val t))
             (run rst t)))
        (else (run rst t))))))

;;; I/O.
(load-shared-object "../common/libnotify/dyn/libnotify.so")

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

(define text '())
(set! text (file->string (get-file-arg-or-exit)))

(notify (format "Chez Scheme\t~s" (get-process-id)))

(run
  (parse text)
  (make-tape (make-vector 1) 0))

(notify "stop")
