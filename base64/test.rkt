#lang racket/base

(require racket/tcp racket/os net/base64)
(#%declare #:unsafe)

(define (verify)
  (for ([(src dst) (in-hash (hash "hello" "aGVsbG8=" "world" "d29ybGQ="))])
    (define encoded (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 src) #"")))
    (when (not (equal? encoded dst))
      (error 'verify "~s != ~s" encoded dst))
    (define decoded (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 dst))))
    (when (not (equal? decoded src))
      (error 'verify "~s != ~s" decoded src))))

(define (notify msg)
  (with-handlers ([exn:fail:network? void])
    (let-values ([(in out) (tcp-connect "localhost" 9001)])
      (display msg out)
      (close-input-port in)
      (close-output-port out))))

(module+ test
  (verify))

(module+ main
  (verify)

  (define STR-SIZE 131072)
  (define TRIES 8192)

  (define str1 (make-bytes STR-SIZE (char->integer #\a)))
  (define str2 (base64-encode str1 #""))
  (define str3 (base64-decode str2))

  (notify (format "Racket\t~s" (getpid)))

  (define-values (s-encoded t-encoded t-encoded-real t-encoded-gc)
    (time-apply
     (lambda ()
       (for/sum ([_ (in-range 0 TRIES)])
         (bytes-length (base64-encode str1 #""))))
     '()))

  (define-values (s-decoded t-decoded t-decoded-real t-decoded-gc)
    (time-apply
     (lambda ()
       (for/sum ([_ (in-range 0 TRIES)])
         (bytes-length (base64-decode str2))))
     '()))

  (notify "stop")

  (printf "encode ~s... to ~s...: ~s, ~s\n"
          (substring (bytes->string/utf-8 str1) 0 4)
          (substring (bytes->string/utf-8 str2) 0 4)
          s-encoded (* t-encoded 0.001))
  (printf "decode ~s... to ~s...: ~s, ~s\n"
          (substring (bytes->string/utf-8 str2) 0 4)
          (substring (bytes->string/utf-8 str3) 0 4)
          s-decoded (* t-decoded 0.001)))
