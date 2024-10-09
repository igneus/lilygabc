;; General-purpose utility functions

(define-module (lilygabc util)
  #:export (flatten
            split-at
            map2)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

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
