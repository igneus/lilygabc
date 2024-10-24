;; General-purpose utility functions

(define-module (lilygabc util)
  #:export (flatten
            split-at
            map2
            map3
            map-with-previous
            irange
            char-range)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

;; List transformations

(define flatten (cut apply append <>))

;; Splits the list, starting a new sublist on each
;; item for which fn is truthy
(define (split-at fn lst)
  (fold
   (lambda (i r)
     (if (or (null? r)
             (fn i))
         (append r (list (list i)))
         (begin
           (append! (last r) (list i))
           r)))
   '()
   lst))

;; Two-dimensional map
(define (map2 fn lst)
  (map (cut map fn <>) lst))

(define (map3 fn lst)
  (map (cut map2 fn <>) lst))

;; Map variant passing to the function two arguments.
;; The second argument is "current", the first "previous".
;; For the first list item "previous" is #nil.
(define (map-with-previous fn lst)
  (pair-fold
   (lambda (sublist r)
     (if (> 2 (length sublist))
         r
         (append r (list (fn (first sublist) (second sublist))))))
   '()
   (cons #nil lst)))

;; Generating lists of value ranges

;; integer range, final element included
(define (irange x y) (iota (+ (- y x) 1) x))

(define (char-range x y)
  (map (lambda (x) (string (integer->char x)))
       (apply irange (map char->integer (list x y)))))
