#lang racket
(define make-stack
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (cond
          ((eqv? msg 'empty?) (null? ls))
           ((eqv? msg 'push!)(set! ls (cons (car args) ls)))
           ((eqv? msg 'top)(if (null? ls) "empty stack: cannot get the top"(car ls)))
           ((eqv? msg 'pop!)(if (null? ls)"empty stack: cannot pop"(set! ls (cdr ls))))
           ((eqv? msg 'top-nth)(let ((num (car args)))
             (let find ((i num) (lss ls))
              (cond
               ((null? lss) "not enough elements")
               ((= i 1) (car lss))
               (else (find (- i 1) (cdr lss)))))))
           (else "unknown operation"))))))
