#lang racket
(require "chapter1.rkt")

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(Jack Sprat could eat no chicken fat)) ;; #t
(lat? '((Jack) Sprat could eat no chicken fat)) ;; #f
(lat? '(Jack (Sprat could) eat no chicken fat)) ;; #f
(lat? '()) ;; #t

(lat? '(bacon and eggs)) ;; #t

;; (cond... ) asks questions
;; (lambda... ) creates a function
;; (define... ) gives it a name

;; else is ALWAYS true :D

                                        ; or ;
(or (null? '()) (atom? '(d e f g))) ;; #t
(or (null? '(a b c)) (null? '())) ;; #t
(or (null? '(a b c)) (null? '(atom))) ;; #f
(or (null? '(a b c)) (null? '(atom)) (null? '())) ;; #t
;; or is variadic too :D

                                        ; member ;
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat))))))) ;; #<void>

(member? 'meat '(mashed potatoes and meat gravy)) ;; #t

;; The First Commandment
;; (preliminary)
;; Always ask null? as the first question in expressing any function.

(member? 'liver '(bagels and lox)) ;; #f

(provide (all-defined-out))
