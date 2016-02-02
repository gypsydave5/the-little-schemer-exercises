#lang racket

(require "chapter1.rkt")

                                        ; rember ;
;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) '())
;;      (else (cond
;;             ((eq? (car lat) a) (cdr lat))
;;             (else (cons (car lat)
;;                         (rember a (cdr lat)))))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

;; The Second Commandment
;; Use cons to build lists

(define rerember
  ;; same thing, but recursive
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (rerember a (cdr lat)))
     (else (cons (car lat) (rerember a (cdr lat)))))))

                                        ; firsts ;
(define firsts2
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts2 (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr (car l)))
                 (seconds (cdr l)))))))

;; The Third Commandment
;; When building a list, describe the first typical element,
;; and then cons it onto the natural recursion.

(define firsts
  (lambda (l)
    (cond
     ((null? l) '()) ;; what we need to cons atoms onto to make a list
     (else (cons
            (car (car l)) ;; typical element
            (firsts (cdr l))))))) ;; natural recursion

                                        ; insertR ;
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons old
                                (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1)
          (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat))
      (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertL new old (cdr lat)))))))

;; The Fourth Commandment
;; (preliminary)
;; Always change at least one argument while recurring.
;; It must be changed to be closer to termination.
;; The changing argument must be tested in the termination condition:
;; when using cdr, test termination with null?

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat)
                 (multisubst new old (cdr lat)))))))

(provide (all-defined-out))
