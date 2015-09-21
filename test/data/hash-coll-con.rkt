#lang racket

(require con/ccon)
(require con/ccon/collaborative)
(require "con-lib.rkt")
(require racket/trace)
(require rackunit)
(require racket/set)
(require math/statistics)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buckets implementation

(define (replicate in n) (map (lambda (x) in) (range n)))

(define make-bucket set)
(define bucket-add  set-add)

(struct buckets (hash hasher) #:transparent)

(define (make-buckets hasher) 
  (buckets (make-immutable-hash '()) hasher))

(define (buckets-ref bkts val)
  (match bkts
    [(buckets hash hasher) 
     (hash-ref hash (hasher val) (make-bucket))]))

(define (buckets-set bkts val)
    (match bkts
      [(buckets hash hasher)
       (let ([hash-code (hasher val)])
         (buckets 
           (hash-set hash hash-code (bucket-add 
                                      (hash-ref hash hash-code (make-bucket)) 
                                      val)) 
           hasher))]))

(define (get-bucket-size bkts val)
  (match bkts
    [(buckets hash hasher)
     (let ([hash-val (hasher val)])
       `(,hash-val . 
         ,(set-count  (hash-ref hash (hasher val) '()))))]))

(define (mk-managed-buckets-setXref mgr)
  (define (report-size mgr)
    (lambda (bkts val)
      ((informant/cc mgr anyc) (get-bucket-size bkts val))))
  (cons
    (checkCon 
      (funcdc2 anyc eager anyc eager (report-size mgr) eager) 
      eager buckets-set dblame)
    (checkCon 
      (funcdc2 anyc eager anyc eager (report-size mgr) eager) 
      eager buckets-ref dblame)))

(define ((within-stddev? mean dev) val)
  (and (<= (- mean dev) val)) (<= val (+ mean dev)))

(define (finalize-hash-count seen _)
  (let* ([seen-sizes  (map cdr (set->list seen))]
         [std-dev     (stddev seen-sizes)]
         [mean-val    (mean seen-sizes)]
         [within-dev? (within-stddev? mean-val std-dev)])
    (foldr (lambda (x t) (and (within-dev? x) t)) #t seen-sizes)))

(define (seen-adder s val)
  (match val
    [`(,hash . 0)     s]
    [`(,hash . 1)     (set-add s val)]
    [`(,hash . ,size)
     (set-add (set-remove s `(,hash . ,(sub1 size))) val)]))

(define (make-test-bucket-map hasher vals)
  (foldr 
    (lambda (val bkts) (buckets-set bkts val))
    (make-buckets hasher)
    vals))

(define (test-buckets-1)
  (make-test-bucket-map (lambda (x) (modulo x 13)) (range 130)))

(define (test-buckets-2)
  (make-test-bucket-map (lambda (x) (modulo x 13)) (range 135)))

(define (test-buckets-3)
  (make-test-bucket-map 
    (lambda (x) (modulo x 13)) 
    (append (list 144 157 170 183) (range 135))))

(define (test-framework bucket-mkr)
  (let* ([mgr (start-contract-manager seen-adder)]
         [bkts (bucket-mkr)]
         [buckets-ref (cdr (mk-managed-buckets-setXref mgr))])
    (buckets-ref bkts 5)
    (buckets-ref bkts 2)
    (buckets-ref bkts 1)
    (buckets-ref bkts 9)
    (manager-finalize/check mgr finalize-hash-count)))

(define (test-01) (test-framework test-buckets-1))
(define (test-02) (test-framework test-buckets-2))
(define (test-03) (test-framework test-buckets-3))

