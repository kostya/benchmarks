#lang racket/base

(require racket/os racket/fixnum racket/tcp racket/unsafe/ops racket/vector racket/list data/queue
         (rename-in racket/unsafe/ops
                    [unsafe-vector*-ref vector-ref]
                    [unsafe-vector*-set! vector-set!]
                    [unsafe-vector*-length vector-length]
                    [unsafe-fx+ +]
                    [unsafe-fx- -]
                    [unsafe-fxmodulo modulo]
                    [unsafe-fx* *]
                    [unsafe-fx< <]
                    [unsafe-fx> >]
                    [unsafe-fx>= >=]
                    [unsafe-fx<= <=]
                    [unsafe-fx= =]))
(#%declare #:unsafe)

(struct Node (children terminal)
  #:mutable
  #:authentic)

(struct Sieve (limit [prime #:mutable])
  #:authentic
  #:guard (lambda (limit prime type-name)
            (values limit (make-vector (+ limit 1) #f))))

(define (calc sieve)
  (define limit (+ (Sieve-limit sieve)))
  (define prime (Sieve-prime sieve))

  (define (to-list)
    (vector-set*! prime 2 #t 3 #t)
    (for/list ([p (in-range 2 (+ limit 1))]
               #:when (vector-ref prime p))
      p))

  (define (omit-squares)
    (let loop ([r 5])
      (define sq (* r r))
      (cond
        [(>= sq limit) r]
        [else (when (vector-ref prime r)
                (let loop ([i sq])
                  (cond
                    [(>= i limit) i]
                    [else (vector-set! prime i #f)
                          (loop (+ i sq))])))
              (loop (+ r 1))])))

  (define (step1 x y)
    (let ([n (+ (* 4 x x) (* y y))])
      (when (and (<= n limit) (or (= (modulo n 12) 1) (= (modulo n 12) 5)))
        (vector-set! prime n (not (vector-ref prime n))))))

  (define (step2 x y)
    (let ([n (+ (* 3 x x) (* y y))])
      (when (and (<= n limit) (= (modulo n 12) 7 ))
        (vector-set! prime n (not (vector-ref prime n))))))

  (define (step3 x y)
    (let ([n (- (* 3 x x) (* y y))])
      (when (and (> x y) (<= n limit) (= (modulo n 12) 11))
        (vector-set! prime n (not (vector-ref prime n))))))

  (define (loop-y x)
    (let loop ([y 1])
      (cond
        [(< (* y y) limit)
         (step1 x y)
         (step2 x y)
         (step3 x y)
         (loop (+ y 1))]
        [else y])))

  (define (loop-x)
    (let loop ([x 1])
      (cond
        [(< (* x x) limit)
         (loop-y x)
         (loop (+ x 1))]
        [else x])))

  (loop-x)
  (omit-squares)
  (to-list))

(define (generate-trie primes)
  (define root (Node (make-hasheq) #f))
  (for ([el (in-list primes)])
    (define head root)
    (define children (Node-children head))
    (define el-str (number->string el))
    (define el-str-len (string-length el-str))
    (for ([ch (in-string el-str)])
      (set! head (hash-ref! (Node-children head) ch (Node (make-hasheq) #f))))
    (set-Node-terminal! head #t))
  root)

(define (find upper-bound prefix)
  (let/cc return
    (define head (generate-trie (calc (Sieve upper-bound #f))))
    (define str-prefix (number->string prefix))
    (for ([ch (in-string str-prefix)])
      (set! head (hash-ref! (Node-children head) ch #f))
      (when (not head)
        (return #f)))

    (define queue (make-queue))
    (enqueue! queue (cons head str-prefix))
    (let loop ([queue queue]
               [result '()])
      (cond
        [(queue-empty? queue) (sort result <)]
        [else
         (define top-prefix (dequeue! queue))
         (define-values (top prefix) (values (car top-prefix) (cdr top-prefix)))
         (for ([(ch v) (in-hash (Node-children top))])
           (enqueue! queue (cons v (string-append prefix (string ch)))))
         (if (Node-terminal top)
           (loop queue (cons (string->number prefix) result))
           (loop queue result))]))))

(define (verify)
  (define left '(2 23 29))
  (define right (find 100 2))
  (when (not (equal? left right))
    (error 'verify "~s != ~s" left right)))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(define UPPER-BOUND 5000000)
(define PREFIX 32338)

(module+ test
  (verify))

(module+ main
  (verify)
  (notify (format "Racket\t~s" (getpid)))
  (define results (find UPPER-BOUND PREFIX))
  (notify "stop")

  (displayln results))
