#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

                                        ; rember* ;
(define rember*
  (lambda (n l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) n) (rember* n (cdr l)))
                       (else (cons (car l) (rember* n (cdr l))))))
     (else (cons (rember* n (car l)) (rember* n (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) old)
                        (cons old (cons new (insertR* new old (cdr l)))))
                       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;; The First Commandment
;; (final version)
;; When recurring on a list of atoms, lat, ask two questions about it:
;; (null? lat) and else.
;; When recurring on a number, n, ask two questions about it:
;; (zero? n) and else.
;; When recurring on a list of S-expressions, l, ask three questions about it:
;; (null? l), (atom? (car l)) and else.

;; The Fourth Commandment
;; (final version)
;; Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use (cdr lat).
;; When recurring on a number, n, use (sub1 n).
;; And when recurring on a list of S-expressions, l, use (car l)
;; and (cdr 1) if neither (null? l) nor (atom? (car l)) are true.
;;
;; It must be changed to be closer to termination.
;; The changing argument must be tested in the termination condition:
;; when using cdr, test termination with null.
;; when using sub1, test termination with zero?

                                        ; occur ;

(define o+
  (lambda (m n)
    (cond
     ((zero? n) m)
     (else (add1 (o+ m (sub1 n)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
                       ((eq? (car l) a)
                        (add1 (occur* a (cdr l))))
                       (else (occur* a (cdr l)))))
     (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) old)
                        (cons new (subst* new old (cdr l))))
                       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                       ((eq? (car l) old)
                        (cons old (cons new (insertL* new old (cdr l)))))
                       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (cond
                       ((eq? (car l) a) #t)
                       (else (member* a (cdr l)))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(define refactored-member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

;; eqlist? - verbose

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

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

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1)
           (number? a2)) (o= a1 a2))
     ((or (number? a1)
          (number? a2)) #f)
     (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ;; both null - #t
     ((and (null? l1) (null? l2)) #t)
     ((and (null? l1) (atom? (car l2))) #f)
     ((null? l1) #f)
     ;; both cars are atoms
     ((and (atom? (car l1)) (atom? (car l2))
           ;; egan? the car and eqlist? the cdr
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))) #t)
     ((atom? (car l1)) #f)
     ((atom? (car l2)) #f)
     ;; both cars are lists
     ;; eqlist? the car and eqlist? the cdr
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

;; we're saying 'if not null, and not atom, then list

;; the second two clauses can be combined as with an or
;; as the only true case where l1 is null is when l2 is null

(define eqlist?-v2
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)

     ((and (atom? (car l1)) (atom? (car l2))
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))) #t)
     ((atom? (car l1)) #f)
     ((atom? (car l2)) #f)

     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

                                        ; equal
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((atom? s1) #f)
     ((atom? s2) #f)
     (else (eqlist? s1 s2)))))

;; which refactors

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

;; and so...

(define eqlist?-v3
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

;; as the cars can either be an atom or a list, but the cdrs can only be a list

;; The Sixth Commandment
;; Simplify only after the function is correct.

                                        ; rember take 3
(define rember-v2
  (lambda (s l)
    (cond
     ((null l) '())
     ((atom? (car l)) (cond
                       ((equal? car l) s) (cdr l)
                       (else (cons (car l) (rember-v2 s (cdr l))))))
     (else (cond
            ((equal? (car l) s) (cdr l))
            (else (cons (car l) (rember-v2 s (cdr l)))))))))

(define rember-v3
  (lambda (s l)
    (cond
     ((null l) '())
     (else (cond
            ((equal? (car l) s) (cdr l))
            (else (cons (car l) (rember-v3 s (cdr l)))))))))

(define rember-v4
  (lambda (s l)
    (cond
     ((null l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l) (rember-v4 s (cdr l)))))))
