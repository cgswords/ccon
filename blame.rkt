#lang racket

(provide
  blame-labels
  build-contract-exn
  invert-blame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A small shape for blame

(struct blame-labels (pos con neg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for predc and to invert blame

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

