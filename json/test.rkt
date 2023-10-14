#lang racket/base

(require racket/flonum racket/tcp racket/file racket/os racket/unsafe/ops json)

(#%declare #:unsafe)
(struct Coordinates (x y z) #:transparent)

(define (calc text)
  (define jobj (bytes->jsexpr text))
  (let-values ([(x y z len) (for/fold ([x 0.0]
                                       [y 0.0]
                                       [z 0.0]
                                       [len 0])
                                      ([coord (hash-ref jobj 'coordinates)])
                              (values (unsafe-fl+ x (hash-ref coord 'x))
                                      (unsafe-fl+ y (hash-ref coord 'y))
                                      (unsafe-fl+ z (hash-ref coord 'z))
                                      (unsafe-fx+ len 1)))])
    (let ([len-fl (->fl len)])
      (Coordinates (unsafe-fl/ x len-fl) (unsafe-fl/ y len-fl) (unsafe-fl/ z len-fl)))))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(define (verify)
  (define right (Coordinates 2.0 0.5 0.25))
  (for ([v '(#"{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"
             #"{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}")])
    (define left (calc v))
    (when (not (equal? left right))
      (error 'verify "~s != ~s" left right))))

(define (read-c path)
  (parameterize ([current-locale "C"])
    (file->bytes path)))

(module+ test
  (verify)
  (define text (read-c "/tmp/1.json"))
  (time (calc text)))

(module+ main
  (verify)
  (define text (read-c "/tmp/1.json"))

  (notify (format "Racket (Staged)\t~s" (getpid)))
  (define results (calc text))
  (notify "stop")

  (displayln results))
