; gabc parsing - pure Scheme, no LilyPond-specific types/functions

(define-module (lilygabc gabc)
  #:export (note-names
            find-clef)
  #:use-module (srfi srfi-1) ; required to use the correct version of `iota`
  #:use-module (ice-9 regex))

(define note-names
  (let*
      ((irange ; integer range, final element included
        (lambda (x y) (iota (+ (- y x) 1) x)))
       (char-range
        (lambda (x y)
          (map integer->char
               (apply irange (map char->integer (list x y)))))))
    (char-range #\a #\m)))

(define (find-clef gabc-str)
  (let ((match (string-match "\\(([cf])([1-4])" gabc-str)))
    (list
     `(type . ,(match:substring match 1))
     `(line . ,(string->number (match:substring match 2)))
     '(b . #f))))
