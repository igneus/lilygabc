;; General-purpose utility functions

(define-module (lilygabc util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

;; List transformations

(define-public flatten (cut apply append <>))

;; Splits the list, starting a new sublist on each
;; item for which fn is truthy
(define-public (split-at fn lst)
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
(define-public (map2 fn lst)
  (map (cut map fn <>) lst))

(define-public (map3 fn lst)
  (map (cut map2 fn <>) lst))

;; Map variant passing to the function two arguments.
;; The second argument is "current", the first "previous".
;; For the first list item "previous" is #nil.
(define-public (map-with-previous fn lst)
  (pair-fold
   (lambda (sublist r)
     (if (> 2 (length sublist))
         r
         (append r (list (fn (first sublist) (second sublist))))))
   '()
   (cons #nil lst)))

;; Generating lists of value ranges

;; integer range, final element included
(define-public (irange x y) (iota (+ (- y x) 1) x))

(define-public (char-range x y)
  (map (lambda (x) (string (integer->char x)))
       (apply irange (map char->integer (list x y)))))

;; Association lists

;; Note: this only replaces entries known to 'a' with matching entries
;; from 'b', entries unknown to 'a' are not added.
;; i.e. semantics don't match Ruby's Hash#merge.
(define-public (alist-merge a b)
  (map
   (lambda (apair)
     (or (assoc (car apair) b)
         apair))
   a))

;; Type predicates

(define-public (string-or-false? x)
  (or (string? x)
      (eq? #f x)))
