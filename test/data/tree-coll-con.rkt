#lang racket

(require con/ccon)
(require con/ccon/collaborative)
(require racket/trace)
(require rackunit)
(require racket/set)

(provide (all-defined-out))

(define dblame (blame-labels 'server 'contract 'client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BST implementation

(define replicate (lambda (in n) (map (lambda (x) in) (range n))))

(struct node               ())
(struct internal-node node (left value right) #:transparent)
(struct empty-node    node ()                 #:transparent)

(define bst-insert
  (lambda (t ins)
    (match (force t)
      [(internal-node l v r) #:when (<= ins (force v))
       (internal-node (bst-insert l ins) v r)]
      [(internal-node l v r)
       (internal-node l v (bst-insert r ins))]
      [(empty-node)  (internal-node (empty-node) ins (empty-node))])))

(define bst-traverse
  (lambda (t)
    (let ((t^ (force t)))
      (match t^
        [(empty-node) t]
        [(internal-node l v r) 
         (begin 
           (bst-traverse l)
           (bst-traverse r)
           t^)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A node-dependent tree contract combinator
(define tree/ndc
  (lambda (ce se cl sl cv sv cr sr)
    (lambda (t b)
        (match t
          [(empty-node)  
           (begin (checkCon (ce t) se '() b) t)]
          [(internal-node l v r)
           (let ((nv (checkCon (cv t) sv v b)))
             (internal-node
               (checkCon (cl nv) sl l b)
               nv
               (checkCon (cr nv) sr r b)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructs for fullness checking

(define all-children-in?
  (lambda (node check-set)
    (match node
      [(internal-node l v r) 
       (and (set-member? check-set l) 
            (set-member? check-set r))]
      [#f #f]
      [else 
       (raise 
         (exn:fail "Parent node was not an internal node" 
                   (current-continuation-marks)))])))

(define (tree-adder)
  (letrec 
    ([parent-map (make-hash '())]
     [add-set-add 
      (lambda (add-set node)
        (match node 
          [(empty-node)
           (set-add add-set node)]
          [(internal-node l v r)
           (begin
             (hash-set! parent-map (equal-hash-code l) node)
             (hash-set! parent-map (equal-hash-code r) node)
             add-set)]))]
     [check-set-add
      (lambda (add-set node)
        (let* ([add-set     (set-add add-set node)]
               [parent-node (hash-ref parent-map (equal-hash-code node) #f)])
          (if (and parent-node (all-children-in? parent-node add-set))
              (check-set-add add-set parent-node)
              add-set)))])
      add-set-add))

(define node/cc
  (lambda (mgr-chan)
    (lambda (in-chans test output-chan)
      (informant/cc 
        mgr-chan
        (pred/cc
          mgr-chan
          (lambda (n)
            (let ([inputs (map cdr (sync-all-channels in-chans))])
              (cond
                [(apply test inputs)
                 (begin
                   (async-write output-chan (car inputs))
                   n)]
                [else #f]))))))))

(define full-leaf/cc
  (lambda (mgr-chan)
    (lambda (i)
      (informant/cc 
        mgr-chan 
        (pred/cc 
          mgr-chan 
          (lambda (n) (begin (channel-put i 0) #t)))))))

(define full-internal/cc
  (lambda (mgr-chan)
    (lambda (i il ir)
      ((node/cc mgr-chan) (list il ir) (lambda (x y) (= x y)) i))))


(define full/a
  (lambda (rec-strat)
    (lambda (manager-chan)
      (let ([empty/c (full-leaf/cc manager-chan)]
            [node/c  (full-internal/cc  manager-chan)])
        (letrec 
          ([full/c (lambda (i) 
                     (let ([il (make-channel)]
                           [ir (make-channel)])
                       (tree/ndc                     
                         (empty/c i)              eager
                         (lambda (_) (full/c il)) rec-strat
                         (node/c i il ir)         async
                         (lambda (_) (full/c ir)) rec-strat)))])
          full/c)))))

(define full/a-E (full/a eager))
(define full/a-A (full/a async))
(define full/a-S (full/a semi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Generation Tools

(define (bst-tree n)
  (foldr
    (lambda (x t) (bst-insert t x))
    (foldr (lambda (x t) (bst-insert t x)) (empty-node) (range (quotient n 2)))
    (drop (range n) (quotient n 2))))

(define bst-10 (bst-tree 10))

(define bst-3 (bst-tree 3))

(define ((flip f) x y) (f y x))

(define full-bst (foldl (flip bst-insert) (empty-node) '(5 3 7 2 4 6 8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Cases

(define (full/test con strat value)
  (let ([manager  (start-contract-manager (tree-adder))]
        [top-chan (make-channel)])
    (begin
      (checkCon ((con manager) top-chan) 
                 strat value dblame)
      (manager-finalize manager))))

(define (full/traverse-test con strat value)
  (let ([manager  (start-contract-manager (tree-adder))]
        [top-chan (make-channel)])
    (begin
      (let ((t (checkCon ((con manager) top-chan) 
                          strat value dblame)))
        (begin
          (bst-traverse t)
          (manager-finalize manager))))))

(define (test-01) (full/test full/a-E eager bst-10))
(define (test-02) (full/test full/a-E eager full-bst))
(define (test-03) (full/test full/a-E eager (bst-tree 1000)))
(define (test-04) (full/test full/a-A async (bst-tree 1000))) ;; Perhaps unexpected results (?)
(define (test-05) (full/test full/a-S eager (bst-tree 1000)))
(define (test-06) (full/test full/a-S eager full-bst))
(define (test-07) (full/traverse-test full/a-S eager (bst-tree 1000)))
(define (test-08) (full/traverse-test full/a-S eager full-bst))
(define (test-09) (full/traverse-test full/a-S eager (bst-tree 3)))

;; (define tree-tests
;;   (test-suite "Tests for trees"
;;     (check-equal?
;;       (checkCon nattree eager bst-10 dblame)
;;       bst-10
;;       "Eager Tree 1")
;; 
;;     (check-equal?
;;       (bst-insm bst-10 11)
;;       (bst-insert bst-10 11)
;;       "Eager Tree 2")
;; 
;;     (check-pred
;;       (lambda (x) (and (node? x) (promise? (node-left x)) (promise? (node-right x))))
;;       (checkCon nattreeS eager bst-10 dblame)
;;       "Semi Tree 1")
;; 
;;     (check-equal?
;;       (checkCon (bounded-bst-E -10000 10000) eager full-bst dblame)
;;       full-bst
;;       "BST Tree 1")
;; 
;;     (check-equal?
;;       (checkCon (predc full?) eager full-bst dblame)
;;       full-bst
;;       "Full Tree 1")
;; 
;;     (check-exn
;;       exn:fail:contract?
;;       (lambda () (checkCon (predc full?) eager bst-3 dblame))
;;       "Full Tree 2")
;; 
;;     (check-equal?
;;       (let* ((i (make-channel))
;;              (tree (checkCon (full/a i) eager full-bst dblame)))
;;         (traverse-d tree))
;;       full-bst
;;       "Full Tree 3")
;; 
;;     (check-equal?
;;       (let* ((i (make-channel))
;;              (tree (checkCon (full/a* i) eager full-bst dblame)))
;;         tree)
;;       full-bst
;;       "Full Tree 3*")
;; 
;;     (check-equal?
;;      (let* ((i (make-channel))
;;             (tree (checkCon (full/a* i) eager bst-3 dblame)))
;;        (void))
;;      (void)
;;      "Full Tree 4*")
;; 
;;     (check-equal?
;;      (let* ((i (make-channel))
;;             (tree (checkCon (full/a i) eager bst-3 dblame)))
;;         (traverse-d tree))
;;      bst-3
;;      "Full Tree 4")
;; 
;; ))
;; 
;; (require rackunit/text-ui)
;; (run-tests tree-tests)
