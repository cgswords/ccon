#lang racket

(provide
  blame-labels
  build-contract-exn
  invert-blame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Next, a small shape for blame

(struct blame-labels (pos con neg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract combinators and helpers for
;; that process.

(define build-contract-exn
  (lambda (value blame)
    (exn:fail:contract
      (format 
        "Contract violation occured with value ~s blaming ~s" 
        value (blame-labels-pos blame))
      (current-continuation-marks))))

(define invert-blame 
  (lambda (blame)
    (blame-labels 
      (blame-labels-neg blame) 
      (blame-labels-con blame) 
      (blame-labels-pos blame))))


