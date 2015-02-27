#lang racket

(require "core.rkt")
(require "blame.rkt")
(require "combinators.rkt")
(require rackunit)

(provide (all-defined-out))

(define stdout (current-output-port))
(define (flush-stdout) (flush-output stdout))

;; This is to deal with async errors 
(uncaught-exception-handler (λ (_) (displayln _) ((error-escape-handler))))

(define dblame (blame-labels 'server 'contract 'client))

(struct node (left value right) #:transparent)

(define treedc
  (lambda (c1 s1 c2 s2 c3 s3 c4 s4)
    (lambda (t b)
      (match t
        [(node l v r) (let ((nv (checkCon c3 s3 v b)))
                        (node
                          (checkCon (c2 nv) s2 l b)
                          nv
                          (checkCon (c4 nv) s4 r b)))]
        [v            (checkCon c1 s1 v b)]))))

(define bst-insert
  (lambda (t ins)
    (match t
      [(node l v r) #:when (<= ins v) (node (bst-insert l ins) v r)]
      [(node l v r)                   (node l v (bst-insert r ins))]
      ['()                            (node '() ins '())]
      [v            #:when (<= v ins) (node '() v (node '() ins '()))]
      [v                              (node (node '() ins '()) v '())])))

(define bst-insert-d
  (lambda (dt ins)
    (match (force dt)
      [(node l v r) #:when (<= ins (force v)) (node (bst-insert-d l ins) v r)]
      [(node l v r)                           (node l v (bst-insert-d r ins))]
      ['()                                    (node '() ins '())]
      [v            #:when (<= (force v) ins) (node '() v (node '() ins '()))]
      [v                                      (node (node '() ins '()) v '())])))

(define (bst-tree n)
  (foldr
    (lambda (x t) (bst-insert t x))
    (foldr (lambda (x t) (bst-insert t x)) '() (range (quotient n 2)))
    (drop (range n) (quotient n 2))))

(define bounded-bst-E
  (lambda (lo hi)
    (treedc
      (predc null?) eager
      (lambda (n) (bounded-bst-E lo n)) eager
      (predc (lambda (x) (and (<= lo x) (<= x hi)))) eager
      (lambda (n) (bounded-bst-E n hi)) eager)))

(define ((flip f) x y) (f y x))

(define full-bst (foldl (flip bst-insert) 5 '(3 7 2 4 6 8)))

(define gen-next (lambda (lo hi) (quotient (+ hi lo) 2)))

(define gen-bst-sequence
  (lambda (hi)
    (let loop ((lo 0) (hi hi))
      (cond
        [(> 2 (- hi lo)) '()]
        [else (let ((next (gen-next lo hi)))
                (append `(,next) (loop lo next) (loop next hi)))]))))

(define build-full-bst
  (lambda (n)
    (let ((elems (gen-bst-sequence (expt 2 n))))
      (foldl (flip bst-insert) (node '() (car elems) '()) (cdr elems)))))

(define (full-check t) 
  (match t
    [(node l v r) (let ((hl (full-check l))
                        (hr (full-check r)))
                    (if (and (number? hl) (number? hr) (= hr hl)) (add1 hl) #f))]
    [_ 0]))

(define (full? t)
   (number? (full-check t)))

(define full/a
  (lambda (i)
    (let ((il (make-channel))
          (ir (make-channel)))
      (treedc                     
        (predc (lambda (n) (begin (channel-put i 0) #t))) eager
        (lambda (_) (full/a il)) semi
        (predc (lambda (n) 
                  (let ((hr (sync (choice-evt ir il)))   
                        (hl (sync (choice-evt ir il))))
                    (if (= hl hr)
                        (begin (channel-put i (add1 hl)) #t)
                        #f))))
        async
        (lambda (_) (full/a ir)) semi))))

(define full/a*
  (lambda (i)
    (let ((il (make-channel))
          (ir (make-channel)))
      (treedc                     
        (predc (lambda (n) (begin (channel-put i 0) #t))) eager
        (lambda (_) (full/a* il)) eager
        (predc (lambda (n) 
                  (let ((hr (sync (choice-evt ir il)))   
                        (hl (sync (choice-evt ir il))))
                    (if (= hl hr)
                        (begin (channel-put i (add1 hl)) #t)
                        #f))))
        async
        (lambda (_) (full/a* ir)) eager))))

(define bst-contains?
  (lambda (dt elem)
    (match (force dt)
      [(node l v r) #:when (= elem (force v)) #t]
      [(node l v r) #:when (< elem (force v)) (bst-contains? l elem)]
      [(node l v r)                           (bst-contains? r elem)]
      ['()                                    #f])))

(define (print-dot) 
  (printf " .") 
  (flush-stdout))

(printf "Building trees for timing tests .")
(define full-bst-1  (build-full-bst  1))
(print-dot)
(define full-bst-2  (build-full-bst  2))
(print-dot)
(define full-bst-3  (build-full-bst  3))
(print-dot)
(define full-bst-4  (build-full-bst  4))
(print-dot)
(define full-bst-5  (build-full-bst  5))
(print-dot) 
(define full-bst-6  (build-full-bst  6))
(print-dot) 
(define full-bst-7  (build-full-bst  7))
(print-dot) 
(define full-bst-8  (build-full-bst  8))
(print-dot)
(define full-bst-9  (build-full-bst  9))
(print-dot)
(define full-bst-10 (build-full-bst 10))
(print-dot)
(define full-bst-11 (build-full-bst 11))
(print-dot)
(define full-bst-12 (build-full-bst 12))
(print-dot)
(define full-bst-13 (build-full-bst 13))
(print-dot)
(define full-bst-14 (build-full-bst 14))
(print-dot)
(define full-bst-15 (build-full-bst 15))
(print-dot)
(define full-bst-16 (build-full-bst 16))
(print-dot)
(define full-bst-17 (build-full-bst 17))
(print-dot)
(define full-bst-18 (build-full-bst 18))
(print-dot)
(define full-bst-19 (build-full-bst 19))
(print-dot)
(define full-bst-20 (build-full-bst 20))
(print-dot)
(define full-bst-21 (build-full-bst 21))
(print-dot)
(define full-bst-22 (build-full-bst 22))
(print-dot)
(define full-bst-23 (build-full-bst 23))
(print-dot)
(define full-bst-24 (build-full-bst 24))
(print-dot)
(printf "~n")

(printf "Done building trees.~n")

(define ((ccon-full-test bst n))
  (let* ((i (make-channel))
         (tree (checkCon (full/a i) eager bst dblame)))
    (bst-contains? tree n)))

(require racket/contract)

(define full/c (flat-named-contract 'full/c full?))

(define ((built-in-full-test bst n)) 
  (let ((tree (contract full/c bst 'pos 'neg))) 
    (bst-contains? tree n)))

(define batch-time
  (lambda (n thk)
    (foldl 
      (lambda (x ls) 
        (let-values (((ans cpu real cpu*) (time-apply thk '())))
          (cons `(,cpu ,real) ls)))
      '()
      (range n))))

(define compute-timing-avgs
  (lambda (ls)
    (let ((len (1.0 . * . (length ls))))
      (map (lambda (x) (/ x len))
        (foldl
          (lambda (n ls)
            `(,(+ (car n) (car ls)) ,(+ (cadr n) (cadr ls))))
          '(0 0)
          ls)))))

(define (tests iter)
  (printf "Running timing tests .")
  (let ((results 
          (foldl
            (lambda (t n ls) 
              (printf " .")
              (let ((num (random n)))
                (append 
                  ls 
                  `((,(batch-time iter (ccon-full-test t n)) ,(batch-time iter (built-in-full-test t n)))))))
            '()
            `(
               ,full-bst-1 
               ,full-bst-2 
               ,full-bst-3 
               ,full-bst-4 
               ,full-bst-5 
               ,full-bst-6 
               ,full-bst-7 
               ,full-bst-8 
               ,full-bst-9 
               ,full-bst-10
               ,full-bst-11
               ,full-bst-12
               ,full-bst-13
               ,full-bst-14
               ,full-bst-15
               ,full-bst-16
               ,full-bst-17
               ,full-bst-18
               ,full-bst-19
               ,full-bst-20
               ,full-bst-21
               ,full-bst-22
               ,full-bst-23
               ,full-bst-24
              )
            (cdr (range 25)))))
    (printf "~n")
    (printf "Computing timing test results.~n")
    (map cons (range 1 (add1 (length results))) (map (lambda (x) (map compute-timing-avgs x)) results))))

(printf "Priming pump for timing tests.~n")

(batch-time 10 (ccon-full-test full-bst-10 10))
(batch-time 10 (built-in-full-test full-bst-10 10))
(batch-time 10 (ccon-full-test full-bst-10 10))
(batch-time 10 (ccon-full-test full-bst-10 10))
(batch-time 10 (built-in-full-test full-bst-10 10))
(batch-time 10 (built-in-full-test full-bst-10 10))
 
(printf "Starting main benchmark.~n")
(tests 100)

