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
        

;; A recursive tree contract combinator
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

;; A dependent tree contract combinator
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
(define leafc (predc null?))

(define nattree  (treerec leafc eager eager natc eager))
(define nattreeS (treerec leafc semi  semi  natc eager))

(define any/c (predc (lambda (x) #t)))

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

(define bounded-bst-E
  (lambda (lo hi)
    (treedc
      (predc null?) eager
      (lambda (n) (bounded-bst-E lo n)) eager
      (predc (lambda (x) (and (<= lo x) (<= x hi)))) eager
      (lambda (n) (bounded-bst-E n hi)) eager)))

(define bounded-bst-S1
  (lambda (lo hi)
    (treedc
      (predc null?) eager
      (lambda (n) (bounded-bst-E lo n)) semi
      (predc (lambda (x) (and (<= lo x) (<= x hi)))) eager
      (lambda (n) (bounded-bst-E n hi)) semi)))

(define bounded-bst-S2
  (lambda (lo hi)
    (treedc
      (predc null?) eager
      (lambda (n) (bounded-bst-E lo n)) semi
      (predc (lambda (x) (and (<= lo x) (<= x hi)))) semi
      (lambda (n) (bounded-bst-E n hi)) semi)))

(define (bst-tree n)
  (foldr
    (lambda (x t) (bst-insert t x))
    (foldr (lambda (x t) (bst-insert t x)) '() (range (quotient n 2)))
    (drop (range n) (quotient n 2))))

;; Note special-cased two-argument func/c. We should fix this in the future.
(define bst-insm 
  (checkCon (funcc2 nattree eager natc eager nattree eager) 
            eager bst-insert dblame))

(define bst-10 (bst-tree 10))

(define bst-3 (bst-tree 3))

(define ((flip f) x y) (f y x))

(define full-bst (foldl (flip bst-insert) 5 '(3 7 2 4 6 8)))


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

(define tree-tests
  (test-suite "Tests for trees"
    (check-equal?
      (checkCon nattree eager bst-10 dblame)
      bst-10
      "Eager Tree 1")

    (check-equal?
      (bst-insm bst-10 11)
      (bst-insert bst-10 11)
      "Eager Tree 2")

    (check-pred
      (lambda (x) (and (node? x) (promise? (node-left x)) (promise? (node-right x))))
      (checkCon nattreeS eager bst-10 dblame)
      "Semi Tree 1")

    (check-equal?
      (checkCon (bounded-bst-E -10000 10000) eager full-bst dblame)
      full-bst
      "BST Tree 1")

    (check-equal?
      (checkCon (predc full?) eager full-bst dblame)
      full-bst
      "Full Tree 1")

    (check-exn
      exn:fail:contract?
      (lambda () (checkCon (predc full?) eager bst-3 dblame))
      "Full Tree 2")

    (check-equal?
      (let* ((i (make-channel))
             (tree (checkCon (full/a i) eager full-bst dblame)))
        (traverse-d tree))
      full-bst
      "Full Tree 3")

    (check-equal?
      (let* ((i (make-channel))
             (tree (checkCon (full/a* i) eager full-bst dblame)))
        tree)
      full-bst
      "Full Tree 3*")

    (check-equal?
     (let* ((i (make-channel))
            (tree (checkCon (full/a* i) eager bst-3 dblame)))
       (void))
     (void)
     "Full Tree 4*")

    (check-equal?
     (let* ((i (make-channel))
            (tree (checkCon (full/a i) eager bst-3 dblame)))
        (traverse-d tree))
     bst-3
     "Full Tree 4")

))

(require rackunit/text-ui)
(run-tests tree-tests)
