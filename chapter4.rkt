#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (p q)
    (cond
     ((zero? q) p)
     (else (add1 (o+ p (sub1 q)))))))

(define o-
  (lambda (p q)
    (cond
     ((zero? q) p)
     (else (sub1 (o- p (sub1 q)))))))

;; The First Commandment
;; (first revision)
;; When recurring on a list of atoms, lat, ask two questions about it:
;; (null? lat) and else
;; When recurring on a number, n, ask two questions about it:
;; (zero? n) and else

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

;; getting ahead of myself...

;; The Fourth Commandment
;; (first revision)
;; Always chaneg at least one argument while recurring.
;; It must be changet to be closer to termination.
;; The changing argument must be tested in the termination condition:
;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?

;; always cuts off excess:
(define tup-minimal+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) '())
     ((null? tup2) '())
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))
;; always breaks when excess:
(define tup-error+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))
;; always extends with the excess
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))

(define gthan
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gthan (sub1 n) (sub1 m))))))

(define lthan
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lthan (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((gthan n m) #f)
     ((lthan n m) #f)
     (else #t))))

(define oexp
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (oexp n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

                                        ; length ;
(define length
  (lambda (lat)
    (cond
     ((empty? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick0index
  (lambda (n lat)
    (cond
     ((zero? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

                                        ; number ;
(define no-nums
  (lambda (lat)
    (cond
     ((empty? lat) '())
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (cons (car lat)
                                  (all-nums (cdr lat))))
       (else (all-nums (cdr lat))))))))

(define egan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)
           (number? a2)) (o= a1 a2))
     ((or (number? a1)
          (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((empty? lat) 0)
     (else
      (cond
       ((egan? (car lat) a)
        (o+ 1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (o= n 1)))

(define rempick2
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (rempick2 (sub1 n) (cdr lat)))))))

(provide (all-defined-out))
