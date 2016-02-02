#lang racket
                                        ; set
(require "chapter1.rkt" "chapter3.rkt")

(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
          (member? a (cdr l))))
     (else
      (or (member? a (car l))
          (member? a (cdr l)))))))

(define set?-v1
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (cond
       ((member? (car lat) (cdr lat)) #f)
       (else (set? (cdr lat))))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset-member?
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset-member? (cdr lat)))
     (else
      (cons (car lat) (makeset-member? (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
                 (makeset (multirember (car lat)
                                       (cdr lat))))))))

(define subset?-v1
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (cond
            ((member? (car set1) set2)
             (subset? (cdr set1) set2))
            (else #f))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset? (cdr set1) set2))
     (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?-v1
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (cond
       ((member? (car set1) set2) #t)
       (else (intersect? (cdr set1) set2)))))))

(define intersect?-v2
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else (intersect? (cdr set1) set2)))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1)
                                      (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

(define complement
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (complement (cdr set1) set2))
     (else (cons (car set1)
                 (complement (cdr set1) set2))))))


(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #f)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (p)
      (car (cdr (cdr p)))))

                                        ; fun
(define fun?
  (lambda (rel)
    (set? (firsts2 rel))))

(define first-revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))))))

(define unreadable-revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (cons (car (cdr (car rel)))
                       (cons (car (car rel)) '()))
                 (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (buid (second pair) (first pair))))

(define revrel
  (lambda (rel)
    ((null? rel) '())
    (else (cons (revpair (car rel))
                (revrel (cdr rel))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
