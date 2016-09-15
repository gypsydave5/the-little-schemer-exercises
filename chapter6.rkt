#lang racket

(require "chapter4.rkt")

;; Whew...
                                        ; shadows
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp))
                                            (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote *)) (and (numbered? (car aexp))
                                            (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp))
                                            (numbered? (car (cdr (cdr aexp)))))))))

(define numbered?-v2
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))))))

;; value

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) (quote o+)) (o+ (value (car nexp))
                                          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote o*)) (o* (value (car nexp))
                                          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote oexp)) (oexp (value (car nexp))
                                                (value (car (cdr (cdr nexp)))))))))

;; The Seventh Commandment
;; Recur on the _subparts_ that are of the same nature:
;; - On the sublists of a list.
;; - On the subexpressions of an arithmetic expression.

(define value-prefix
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) (quote o+)) (o+ (value (car (cdr nexp)))
                                          (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) (quote o*)) (o* (value (car (cdr nexp)))
                                          (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) (quote oexp)) (oexp (value (car (cdr nexp)))
                                          (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp-v1
  (lambda (aexp)
    (cond
     (else (car (cdr aexp))))))

;; or

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-prefix-v2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote o+)) (o+ (value (1st-sub-exp nexp))
                                           (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote o*)) (o* (value (1st-sub-exp nexp))
                                           (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote oexp)) (oexp (value (1st-sub-exp nexp))
                                               (value (2nd-sub-exp nexp)))))))

(define 1st-sub-exp-infix
  (lambda (aexp) (car aexp)))

(define 2nd-sub-exp-infix
  (lambda (aexp) (car (cdr (cdr aexp)))))

(define operator-infix
  (lambda (aexp) (car (cdr aexp))))

(define value-infix
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator-infix nexp) (quote o+)) (o+ (value (1st-sub-exp-infix nexp))
                                           (value (2nd-sub-exp-infix nexp))))
     ((eq? (operator-infix nexp) (quote o*)) (o* (value (1st-sub-exp-infix nexp))
                                           (value (2nd-sub-exp-infix nexp))))
     ((eq? (operator-infix nexp) (quote oexp)) (oexp (value (1st-sub-exp-infix nexp))
                                               (value (2nd-sub-exp-infix nexp)))))))

;; The Eighth Commandment
;; Use help functions to abstract from representations.

(define value-v2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((car (cdr nexp))
                 (value (car nexp))
                 (value (car (cdr (cdr nexp)))))))))

;; Numbers are representations - why 4? Why not (() () () ())?

                                        ; A Whole New World
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define oo+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (oo+ n (zub1 m)))))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;; (edd1 '()) ;; (())
;; (zub1 '(() () ())) ;; (() ())
;; (sero? '()) ;; #t
;; (sero? '(())) ;; #f
;; (oo+ '(() () ()) '(() ())) ;; (() () () () ())

;; (lat? '(1 2 3))
;; ;; #t
;; (lat? '(() () ()))
;; #f

;;  You must beware of shadows...
;;  These things aren't real - they are representations...
;;  ... but even the representation of the representations are representations
;;  ... and have meaning

(provide (all-defined-out))
