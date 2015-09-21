#lang racket

(require con/ccon)
(require racket/trace)
(require rackunit)

(provide (all-defined-out))

;; Just some nice helpers for writing contracted code. 
;; Mostly just to save me some boilerplate.

(define dblame (blame-labels 'server 'contract 'client))

(define orf
  (lambda (f1 f2)
    (lambda (v) (or (f1 v) (f2 v)))))

(define nat?  (lambda (n) (> n -1)))
(define natc  (predc nat?))
(define anyc  (predc (lambda (x) #t)))
