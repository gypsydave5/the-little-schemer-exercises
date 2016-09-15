#lang racket
                                        ; ... and Again, and Again, and Again, ...

(require "chapter7.rkt" "chapter4.rkt")

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

;; we call this 'unnatural' recursion... mwhahahahhaaa
;; this is a partial function
;;
;; so is this...

(define eternity
  (lambda (x) (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

;; unnatural recursion... may never end

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (length* (first pora))
          (length* (second pora)))))))
