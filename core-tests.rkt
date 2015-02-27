#lang racket

(require "core.rkt")
(require "blame.rkt")
(require "combinators.rkt")
(require rackunit)

;; #lang racket
;;  
;; (uncaught-exception-handler (λ (_) (displayln _) ((error-escape-handler))))
;;   
;; (with-handlers ([void displayln])
;;   (void (thread (λ () (error 'fail)))))

(define dblame (blame-labels 'server 'contract 'client))

(define nat? (lambda (n) (> n -1)))

(define natToNatE (funcc (predc nat?) eager (predc nat?) eager))
(define natToNatS (funcc (predc nat?) semi (predc nat?) semi))
(define natToNatF (funcc (predc nat?) future (predc nat?) future))
(define natToNatA (funcc (predc nat?) async (predc nat?) async))

(define any/c (predc (lambda (x) #t)))

(define fact
  (lambda (n)
    (cond
      [(= n 10) -10]
      [(zero? n) 1]
      [else (* n (fact (- n 1)))])))

(define factF
  (lambda (n)
    (let ((n (force n)))
    (cond
      [(= n 10) -10]
      [(zero? n) 1]
      [else (* n (factF (- n 1)))]))))

(define fact/mE (checkCon natToNatE eager fact dblame))
(define fact/mS (checkCon natToNatS eager factF dblame))
(define fact/mF (checkCon natToNatF eager factF dblame))

(define core-test
  (test-suite "Tests for core forms" 
    (check-equal?
      (checkCon (predc nat?) eager 5 dblame)
      5 
      "Eager 1")
    
    (check-exn
      exn:fail:contract?
      (lambda () (checkCon (predc nat?) eager -1 dblame))
      "Eager 2")

    (check-equal? 
      (fact/mE 5) 
      120 
      "Fact-Eager 1" )

    (check-exn 
      exn:fail:contract?
      (lambda () (fact/mE 10))
      "Fact-Eager 2")

    (check-exn 
      exn:fail:contract?
      (lambda () (fact/mE -1))
      "Fact-Eager 3")

    (check-pred
      promise?
      (checkCon (predc nat?) semi 5 dblame) 
      "Semi-Eager 1")
    
    (check-pred
      promise?
      (checkCon (predc nat?) semi -1 dblame) 
      "Semi-Eager 2")

    (check-equal?
      (force (checkCon (predc nat?) semi 5 dblame))
      5
      "Semi-Eager 3")

    (check-exn 
      exn:fail:contract?
      (lambda () (force (checkCon (predc nat?) semi -1 dblame)))
      "Semi-Eager 4")

    (check-equal? 
      (force (fact/mS 5)) 
      120 
      "Fact-Semi 1" )

    (check-exn 
      exn:fail:contract?
      (lambda () (force (fact/mS 10)))
      "Fact-Semi 2")

    (check-exn 
      exn:fail:contract?
      (lambda () (force (fact/mS -1)))
      "Fact-Semi 3")

    (check-pred
      promise?
      (checkCon (predc nat?) future 5 dblame)
      "Future 1")

    (check-pred
      promise?
      (checkCon (predc nat?) future -1 dblame)
      "Future 2")

    (check-equal?
      (force (checkCon (predc nat?) future 5 dblame))
      5
      "Future 3")

    (check-exn
      exn:fail:contract?
      (lambda () (force (checkCon (predc nat?) future -1 dblame))) 
      "Future 4")

    (check-equal?
      (force (fact/mS 5))
      120
      "Fact-Future 1")
 
    (check-exn
      exn:fail:contract?
      (lambda () (force (fact/mS 10))) 
      "Fact-Future 2")

    (check-exn
      exn:fail:contract?
      (lambda () (force (fact/mS -1))) 
      "Fact-Future 3")

    (check-equal?
      (checkCon (predc nat?) async 5 dblame)
      5
      "Async 1")

;; There isn't a good way to catch these exceptions in Racket. That's probably okay.
;; (testContract "Async 1" (checkCon (predc nat?) async -1 dblame) 
;;               "Contract violation occured with value -1 blaming client") 
    
    ))

(require rackunit/text-ui)
(run-tests core-test)
