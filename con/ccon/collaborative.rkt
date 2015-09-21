#lang racket

(require "manager.rkt")
(require "core.rkt")
(require "combinators.rkt")
(require "blame.rkt")
(require racket/trace)
(require rackunit)
(require racket/set)

(provide 
  async-write
  sync-all-channels
  start-contract-manager
  informant/cc
  pred/cc
  manager-add
  manager-checked
  manager-error
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to supplement the built-in communication facilities

(define (async-write channel value)
  (thread (lambda () (channel-put channel value))))

(define (sync-all-channels in-chans)
  (let ([master-chan (make-channel)])
    (define ((channel-handler chan)) (channel-put master-chan `(,chan . ,(channel-get chan))))
    (let ([master-chan (make-channel)])
      (begin
        (map (lambda (chan) (thread (channel-handler chan))) in-chans)
        (let loop ((chans in-chans) (results '()))
          (cond
            [(null? in-chans) results]
            [else (let ([result (sync master-chan)])
                    (if (memq (car result) in-chans)
                        (loop (remq (car results) in-chans) (cons result results))
                        (loop in-chans results)))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collaborative Contracts

(define (informant/cc mgr con)
  (lambda (information)
    (lambda (value blame)
      (begin
        (manager-add mgr information)
        (let ([res (con value blame)])
          (begin
            (manager-checked mgr information)
            res))))))

(define (pred/cc mgr f)
  (lambda (value blame)
    (let ([x value])
      (cond
        [(f x) x]
        [else (begin (manager-error mgr x)
                     (raise (build-contract-exn x blame)))]))))


