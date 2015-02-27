#lang racket

(provide
  predc
  funcc
  funcc2)

(require "core.rkt")
(require "blame.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
(define predc
  (lambda (f)
    (lambda (v blame)
      (let ((x v))
        (if (f x)
            x
            (raise (build-contract-exn x blame)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Contracts

(define funcc
  (lambda (c1 s1 c2 s2)
    (lambda (v blame)
      (let ((f v))
       (if (procedure? f)
            (lambda (x) 
              (checkCon c2 s2
                (f (checkCon c1 s1 x (invert-blame blame)))
                blame))
            (build-contract-exn v))))))

(define funcc2
  (lambda (c1 s1 c2 s2 c3 s3)
    (lambda (v blame)
      (let ((f v))
       (if (procedure? f)
            (lambda (x y) 
              (checkCon c3 s3
                (f (checkCon c1 s1 x (invert-blame blame)) 
                   (checkCon c2 s2 y (invert-blame blame)))
                blame))
            (build-contract-exn v))))))

