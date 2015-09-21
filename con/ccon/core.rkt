#lang racket

(require "manager.rkt")

(provide
  eager
  semi
  future
  async
  fsync
  start-fsync-manager
  manager-finalize
  manager-finalize/check
  checkCon
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The implementation for contract checking using the semantics presented in
;; Expressing Contract Monitors as Patterns of Communication, using racket's
;; built-in thread and communication channel forms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, we'll build our individual
;; strategies and their user-exposed 
;; invocations. In a module setting, the
;; empty structs should not be exposed.

(struct eager-strat  ())
(struct semi-strat   ())
(struct future-strat ())
(struct async-strat  ())
(struct fsync-strat  (mgr))

(define eager       (eager-strat))
(define semi        (semi-strat))
(define future      (future-strat))
(define async       (async-strat))
(define (fsync mgr) (fsync-strat mgr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's put things in left and right 

(struct left (val))
(struct right (val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The actual checker. It's just a macro
;; that cases on the strategy passed in.

;; The error propagation currently works fine
;; for everything except async, which does...
;; bad things.

(define raiseM
  (lambda (x f)
    (match x
      [(left v) (f v)]
      [(right v) (raise v)])))

(define id (lambda (x) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finally synchronous infrastructure 
;; (built from collaborative checking manager)

(define (start-fsync-manager) 
  (start-contract-manager set-add))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-syntax checkCon
;;   (syntax-rules () 
;;       [(_ c s e b)
;;        (cond
;;          [(eager-strat? s)  ...]
;;          [(semi-strat? s)   ...]         
;;          [(future-strat? s) ...]
;;          [(async-strat? s) 
;;           (begin
;;               (thread (lambda () (c e b)))
;;               e)])]))

;; This is just to make the definition below a 
;; little nicer to read
(define-syntax mk-thread
  (syntax-rules ()
   [(_ e) (thread (lambda () e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The real magic
(define-syntax checkCon
  (syntax-rules () 
      [(_ c s e b)
       (match s
         [(eager-strat) 
          (let ([i (make-channel)])
            (begin
              (mk-thread 
                (channel-put i 
                  (with-handlers 
                    ([exn:fail:contract? right]) 
                    (left (c e b)))))
              (raiseM (channel-get i) id)))]
         [(semi-strat) 
          (let ([i (make-channel)])
            (begin
              (mk-thread
                (channel-put i 
                  (with-handlers 
                    ([exn:fail:contract? right]) 
                    (left (delay (c e b))))))
              (raiseM (channel-get i) id)))]         
         [(future-strat) 
          (let ([i (make-channel)])
            (begin
              (mk-thread
                (channel-put i 
                  (with-handlers 
                  ([exn:fail:contract? right]) 
                  (left (c e b)))))
              (delay (raiseM (channel-get i) id))))]
         [(async-strat) 
          (let ([i (make-channel)])
            (begin
              (mk-thread (c e b))
              e))]
         [(fsync-strat mgr)
          (begin
            (mk-thread
              (let ([v e])
                (manager-add mgr v)
                (with-handlers
                  ([exn:fail:contract? 
                     (lambda (exn) (manager-error mgr exn b))])
                  (c v b))
                (manager-checked mgr v)))
            e)])]))

