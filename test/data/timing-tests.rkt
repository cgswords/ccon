#lang racket

(require con/ccon)
(require racket/trace)
(require rackunit)

(provide (all-defined-out))

;; (provide
;;   treec
;;   treerec
;;   bst-tree
;;   bst-insert
;;   bst-insert-d
;;   bst-insm
;;   nattreeS
;;   node
;;   leaf
;;   nil)

;; #lang racket
;;  
;; (uncaught-exception-handler (λ (_) (displayln _) ((error-escape-handler))))
;;   
;; (with-handlers ([void displayln])
;;   (void (thread (λ () (error 'fail)))))

(define dblame (blame-labels 'server 'contract 'client))

(struct node (left value right) #:transparent)

;; An utterly useless tree combinator!
(define treec
  (lambda (c1 s1 c2 s2 c3 s3 c4 s4)
    (lambda (t b)
      (match t
        [(node l v r) (let ((nv (checkCon c3 s3 v b)))
                        (node
                          (checkCon c2 s2 l b)
                          nv
                          (checkCon c4 s4 r b)))]
        [v            (checkCon c1 s2 v b)]))))
        

(define treerec
  (lambda (c1 s1 s2 c3 s3)
    (letrec 
      ((treec (lambda (t b)
                (match t
                  [(node l v r) (let ((nv (checkCon c3 s3 v b)))
                                  (node
                                    (checkCon treec s2 l b)
                                    nv
                                    (checkCon treec s2 r b)))]
                  [v            (checkCon c1 s2 v b)]))))
                  
      treec)))

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

(define orf
  (lambda (f1 f2)
    (lambda (v) (or (f1 v) (f2 v)))))

(define nat?  (lambda (n) (> n -1)))
(define natc  (predc nat?))
(define leafc (predc (orf null? nat?)))

(define nattree  (treerec leafc eager eager leafc eager))
(define nattreeS (treerec leafc semi semi leafc eager))

;; (define any/c (predc (lambda (x) #t)))

(define traverse-d
  (lambda (dt)
    (let ((t (force dt)))
      (match t
        [(node l v r) (node (traverse-d l) v (traverse-d r))]
        [v (force v)]))))

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

;; Note special-cased two-argument func/c. We should fix this in the future.
(define bst-insm 
  (checkCon (funcc2 nattree eager natc eager nattree eager) 
            eager bst-insert dblame))

(define bst-10 (bst-tree 10))

(define bst-3 (bst-tree 3))

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

(define big-full-bst (build-full-bst 20))

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

;;(time
;;  (let* ((i (make-channel))
;;         (tree (checkCon (full/a* i) eager big-full-bst dblame)))
;;    (bst-contains? tree 30000)))



(printf "Building trees for timing tests.~n")
(define full-bst-1  (build-full-bst  1))
(define full-bst-2  (build-full-bst  2))
(define full-bst-3  (build-full-bst  3))
(define full-bst-4  (build-full-bst  4))
(define full-bst-5  (build-full-bst  5))
(define full-bst-6  (build-full-bst  6))
(define full-bst-7  (build-full-bst  7))
(define full-bst-8  (build-full-bst  8))
(define full-bst-9  (build-full-bst  9))
(define full-bst-10 (build-full-bst 10))
(define full-bst-11 (build-full-bst 11))
(define full-bst-12 (build-full-bst 12))
(define full-bst-13 (build-full-bst 13))
(define full-bst-14 (build-full-bst 14))
(define full-bst-15 (build-full-bst 15))
(define full-bst-16 (build-full-bst 16))
(define full-bst-17 (build-full-bst 17))
(define full-bst-18 (build-full-bst 18))
(define full-bst-19 (build-full-bst 19))
(define full-bst-20 (build-full-bst 20))
(define full-bst-21 (build-full-bst 21))
(define full-bst-22 (build-full-bst 22))
(define full-bst-23 (build-full-bst 23))
(define full-bst-24 (build-full-bst 24))

(define ((ccon-full-test bst))
  (let* ((i (make-channel))
         (tree (checkCon (full/a i) eager bst dblame)))
    (bst-contains? tree 30000)))

(require racket/contract)

(define full/c (flat-named-contract 'full/c full?))

(define ((built-in-full-test bst)) 
  (let ((tree (contract full/c bst 'pos 'neg))) 
    (bst-contains? tree 30000)))


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
  (printf "Starting the timing tests.~n")
  (let ((results 
          (foldl
            (lambda (t ls) 
              (append 
                ls 
                `((,(batch-time iter (ccon-full-test t)) ,(batch-time iter (built-in-full-test t))))))
            '()
            `(,full-bst-1 
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
              ))))
    (printf "Computing timing test results.~n")
    (map cons (range 1 (add1 (length results))) (map (lambda (x) (map compute-timing-avgs x)) results))))

(printf "Priming pump for timing tests.~n")

(batch-time 10 (ccon-full-test full-bst-10))
(batch-time 10 (built-in-full-test full-bst-10))
(batch-time 10 (ccon-full-test full-bst-10))
(batch-time 10 (ccon-full-test full-bst-10))
(batch-time 10 (built-in-full-test full-bst-10))
(batch-time 10 (built-in-full-test full-bst-10))
 
(tests 100)

