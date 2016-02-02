#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

                                        ; atoms
(atom? 'atom) ;; #t
(atom? (quote turkey)) ;; #t
(atom? 1492) ;; #t
(atom? '1492) ;; #t
(atom? 'u) ;; #t
(atom? '*abc$) ;; #t

                                        ; lists
(list? '(atom)) ;; #t
(list? '(atom turkey or)) ;; #t
;; (list? '(atom turkey) or) ;; error!
(list? '((atom turkey) or)) ;; #t

                                        ; sexps
;; everything's an S-expression
(list? (quote (how are you doing so far))) ;; #t
;; that list has six S-expressions in it
(list? '(((how) are) ((you) (doing so)) far)) ;; #t
;; that list has threee S-expressions in it

                                        ; emptiness
(list? '()) ;; #t
(list? (quote ())) ;; #t
;; it's the empty list!
(atom? '()) ;; #f
(list? '(() () ())) ;; #t
;; a list of empty lists - number of S-expressions: three

                                        ; car
(car '(a b c)) ;; a
(car '((a b c) x y z)) ;; (a b c)
;; (car 'hotdog) ;; error!
;; (car '()) ;; error!

;; The Law of Car
;; car is defined only for non-empty lists

(car '(((hotdogs)) (and) (pickle) relish)) ;; ((hotdogs))
(car (car '(((hotdogs)) (and)))) ;; (hotdogs)

                                        ; cdr
(cdr '(a b c)) ;; (b c)
(cdr '((a b c) x y z)) ;; (x y z)
(cdr '(hamburger)) ;; ()
(cdr '((x) t r)) ;; (t r)
;; (cdr 'a) ;; error!
;; (cdr '()) ;; error!

;; The Law of Cdr
;; Cdr is defined only for non empty lists
;; The cdr of any non empty lisp is always another list

                                        ; car & cdr

(car (cdr '((b) (x y) ((c))))) ;; (x y)
(cdr (cdr '((b) (x y) ((c))))) ;; (((c)))
;; (cdr (car '(a (b (c)) d))) ;; error!

; car takes a non-empty list; cdr takes a non-empty list.

                                        ; cons
(cons 'peanut '(butter and jelly)) ;; (peanut butter and jelly)
;; read `cons a onto l`

(cons '(banana and) '(peanut butter and jelly)) ;; ((banana and) peanut butter and jelly)
;; cons takes two arguments i) any S-expression, ii) any list.

(cons '(a b (c)) '()) ;; ((a b (c)))
(cons 'a '()) ;; (a)
(cons '((a b c)) 'b) ;; (((a b c)) . b) - but they want you to believe it won't...
(cons 'a 'b) ;; (a . b) - it's a cons cell, but they don't want you to see these...

;; The Law of Cons
;; The primitive cons takes two arguments.
;; The second argument to cons must be a list.
;; The result is a list

(cons 'a (car '((b) c d))) ;; (a b)
(cons 'a (cdr '((b) c d))) ;; (a c d)
                                        ; null?

(null? '()) ;; #t
(null? (quote ())) ;; #t
(null? '(a b c)) ;; #f
(null? 'a) ;; #f

;; The Law of Null?
;; The primitive null? is defined only for lists.

(atom? 'Harry) ;; #t
(atom? '(Harry had a heap of apples)) ;; #f
;; atom takes one argument; any S-expression.

(atom? (car '(Harry had a heap of apples))) ;; #t
(atom? (cdr '(Harry had a heap of apples))) ;; #f
(atom? (cdr '(Harry))) ;; #f
(atom? (car (cdr '(swing low sweet cherry oat)))) ;; #t
(atom? (car (cdr '(swing (low sweet) cherry oat)))) ;; #f

                                        ; equality ;
(eq? 'Harry 'Harry) ;; #t
(eq? 'margarine 'butter) ;; #f
;; eq takes two arguments, bothe non-numeric atoms
(eq? '() '()) ;; #t
(eq? '(1) '()) ;; #f
(eq? 6 7) ;; #f (although we shouldn't rely on it...)
(eq? -1 1) ;; #f
(eq? 1.1 1.1) ;; #t
(eq? 1.2 1.1) ;; #f

;; The Law of Eq?
;; The primative eq? takes two arguments.
;; Each must be a non-numeric atom.

(eq? (car '(Mary had a little lamb chop)) 'Mary) ;; #t
(eq? (cdr '(soured milk)) 'milk) ;; #f
(eq? (car '(beans beans we need jelly beans))
     (car (cdr '(beans beans we need jelly beans)))) ;; #t

(provide (all-defined-out))
