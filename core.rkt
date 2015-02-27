#lang racket

(provide
  eager
  semi
  future
  async
  checkCon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The implementation for contract checking using the semantics presented in
;; Contract Monitoring Semantics as Patterns of Communication, using racket's
;; built-in thread and communication channel forms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, we'll build our individual
;; strategies and their user-exposed 
;; invocations.

(struct eager-strat ())
(struct semi-strat ())
(struct future-strat ())
(struct async-strat ())
(struct fsync-strat ())

(define eager  (eager-strat))
(define semi   (semi-strat))
(define future (future-strat))
(define async  (async-strat))
(define fsync  (async-strat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's put things in left and right 

(struct left (val))
(struct right (val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The actual checker. It's just a macro
;; that examines the strategy passed in.

(define catchM
  (lambda (x f)
    (match x
      [(left v)  (f v)]
      [(right v) (raise v)])))

(define id (lambda (x) x))

(define-syntax checkCon
  (syntax-rules () 
      [(_ c s e b)
       (cond
         [(eager-strat? s) 
          (let ((i (make-channel)))
            (begin
              (thread (lambda () 
                        (channel-put i 
                          (with-handlers 
                            ([exn:fail:contract? (lambda (exn) (right exn))]) 
                            (left (c e b))))))
              (catchM (channel-get i) id)))]
         [(semi-strat? s) 
          (let ((i (make-channel)))
            (begin
              (thread (lambda () 
                        (channel-put i 
                          (with-handlers 
                            ([exn:fail:contract? (lambda (exn) (right exn))]) 
                            (left (delay (c e b)))))))
              (catchM (channel-get i) id)))]         
         [(future-strat? s) 
          (let ((i (make-channel)))
            (begin
              (thread (lambda () 
                        (channel-put i 
                          (with-handlers 
                            ([exn:fail:contract? (lambda (exn) (right exn))]) 
                            (left (c e b))))))
              (delay (catchM (channel-get i) id))))]
         [(async-strat? s) 
          (let ((i (make-channel)))
            (begin
              (thread (lambda () (c e b)))
              e))])]))
