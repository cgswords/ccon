#lang racket

(provide
  start-contract-manager
  manager-finalize
  manager-finalize/check
  manager-add
  manager-checked
  manager-error
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrastructure for collaborative checking manager
(struct add-instr      (val))
(struct checked-instr  (val))
(struct error-instr    (value blame))
(struct finalize-instr (proc))

(struct manager-set (manager-chan finalize-chan))

(define sync-manager
  (lambda (chan fchan checkable-set-add)
    (let loop ((checkable-set (set)) 
               (checked-set (set)) 
               (finalize? #f)
               (fproc (lambda (x y) 'finalized))
               (error? #f))
      (cond 
        [(and finalize? error?) 
         (channel-put fchan error?)]
        [(and finalize? (set=? checkable-set checked-set))
         (channel-put fchan (fproc checkable-set checked-set))]
        [else 
          (let ((instr (channel-get chan)))
            (match instr
              [(add-instr val) 
                (loop (checkable-set-add checkable-set val)
                      checked-set
                      finalize? fproc error?)]
              [(checked-instr val)
               (loop checkable-set (set-add checked-set val) finalize? fproc error?)]
              [(error-instr value blame) 
               (loop checkable-set checked-set finalize? fproc `(error . ,value))]
              [(finalize-instr p) 
               (loop checkable-set checked-set #t p error?)]))]))))

(define (start-contract-manager adder)
  (let ([manager-chan  (make-channel)]
        [finalize-chan (make-channel)])
    (begin
      (thread (lambda () (sync-manager manager-chan finalize-chan adder)))
      (manager-set manager-chan finalize-chan))))

(define (manager-finalize mgr-set)
  (match mgr-set
    [(manager-set mgr-chan fchan)
     (begin
       (channel-put mgr-chan (finalize-instr (lambda (x y) 'finalized)))
       (channel-get fchan))]))

(define (manager-finalize/check mgr-set checker)
  (match mgr-set
    [(manager-set mgr-chan fchan)
     (begin
       (channel-put mgr-chan (finalize-instr checker))
       (channel-get fchan))]))

(define (manager-add mgr val)
  (match mgr
    [(manager-set mgr-chan fchan)
     (channel-put mgr-chan (add-instr val))]))

(define (manager-checked mgr val)
  (match mgr
    [(manager-set mgr-chan fchan)
     (channel-put mgr-chan (checked-instr val))]))

(define (manager-error mgr val blame)
  (match mgr
    [(manager-set mgr-chan fchan)
     (channel-put mgr-chan (error-instr val blame))]))

