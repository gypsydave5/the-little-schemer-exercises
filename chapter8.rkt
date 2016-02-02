#lang racket
                                        ; lambda the ultimate
(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f-c
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? a (car l)) (cdr l))
       (else (cons (car l)
                   ((rember-f-c test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons new (cons old (cdr l))))
       (else (cons (car l)
                   ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons old (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old) (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define subst
  (insert-g (lambda (new old l)
              (cons new l))))

(define seqrem
  (lambda (new old l) l))

(define rember-v1
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define rember
  (lambda (a l)
    ((insert-g (lambda (_ __ l) l)) #f a l)))

;; The Ninth Commandment
;; Abstract common patterns with a new function

(require "chapter6.rkt" "chapter4.rkt")

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) o+)
     ((eq? x '*) o*)
     (else  oexp))))

(define value-v3
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
        ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

(define multirember-T
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multirember-T test? (cdr lat)))
     (else (cons (car lat)
                 (multirember-T test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

;; I feel like I want to write a short essay about this function. Possibly a poem about how clever
;; it is. Maybe we'll go with the essay instead...
;;
;; So we start with the escape/end case, when we've run out of items in the list - and at that moment
;; we run `col` - whatever the hell `col` is, with two empty lists as its arguments. If we start with
;; an empty list as `lat`, we end up with running `col` against two empty lists. Sweet
;;
;; So now it gets fun. If the current value of `(car lat)` is equal to `a`, we recur. We recur with,
;; well, with `a` in the first position, and with the `cdr` of `lat` in the second position. So far
;; so normal (ish). The magic is in the third positon. We create a new value of `col`, taking two
;; arguments, `newlat` and `seen`. And in that lambda, we execute the current value of col with a
;; couple of new arguments - the current value of `newlat` passed to the lambda, and
;; `(cons (car lat) seen)` now appearing as `seen`. So, in other words, the value of `seen` gets the
;; the value of `a` consed onto the front of it - we could as easily write `(cons a seen)`
;;
;; We do something similar with the `else` path, but instead of `cons`ing onto `seen` we cons onto
;; `newlat`.
;; The magic here is that the `cons` _only evaluates at the very end of the recursion_ - and when it
;; does it 'collects' up all the values consed with `newlat` and `seen` until it gets to the original
;; `col` with a pair of lists ready for `col` to do with them whatever it's going to do.
;;
;; Imagining this situation: (multirember&co 'tuna '(tuna bake) a-friend)
;; at the end the call-stack... a lambda that gets called with two empty lists...
;; ... that calls a lambda with `bake` consed onto the first argument (an empty list), and the second argument (an empty list)...
;; that calls another lambda with the first argument `(cons 'bake '())` passed through and the second
;; argmument of `tuna` consed onto the second argument passed through `(cons 'tuna '())`
;; ... which finally gets as far up as `a-friend`, with `x` as `(cons 'bake '())` and `y` as `(cons 'tuna '())`
;;  which is false as '(tuna) isn't empty.
;;  result is generated through a recursively defined function, only called at the end of the
;; list, which calls the original function given to it at the end:
;; end of list ->> generate -> the -> data -> and ->> call the original `col` function with it.

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new
                                 oldL
                                 oldR
                                 (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new
                                 oldL
                                 oldR
                                 (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertLR new
                                oldL
                                oldR
                                (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new
                                     (cons oldL newlat))
                               (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR
                                     (cons new newlat))
                               L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat) L R)))))))

;; I think it is worth remembering that I came up with those lambdas myself

(define totes-change
  (lambda (newlat L R)
    (+ L R)))

(define new-lat
  (lambda (newlat L R)
    newlat))

(define even?
  (lambda (n)
    (= (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l)
              (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l) (lambda (newlist p s)
                                  (col (cons (car l) newlist)
                                       (o* p (car l)) s))))
       (else
        (evens-only*&co (cdr l) (lambda (newlist p s)
                                  (col newlist
                                       p
                                       (o+ s (car l))))))))
     (else
      (evens-only*&co (car l) (lambda (al ap as)
                                ;; a is for car
                                ;; we move down the car
                                ;; and at th end of the car... (col '() 1 0)
                                ;; back up the car
                                ;; and at the top of the car
                                (evens-only*&co (cdr l)
                                                (lambda (dl dp ds)
                                                  ;; d is for cdr
                                                  ;; when we've come up from the car
                                                  ;; we move across the cdr by executing
                                                  ;; this lambda... eventually...
                                                  ;; after moving all the way across
                                                  ;; to (col '() 1 0)
                                                  (col (cons al dl)
                                                       (o* ap dp)
                                                       (o+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))
