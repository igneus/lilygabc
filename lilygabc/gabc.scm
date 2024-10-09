; gabc parsing - pure Scheme, no LilyPond-specific types/functions

(define-module (lilygabc gabc)
  #:export (note-names
            find-clef
            parse)
  #:use-module (srfi srfi-1) ; required to use the correct version of `iota`
  #:use-module (ice-9 regex))

; list of gabc note names, in an ascending order, as strings
(define note-names
  (let*
      ((irange ; integer range, final element included
        (lambda (x y) (iota (+ (- y x) 1) x)))
       (char-range
        (lambda (x y)
          (map (lambda (x) (string (integer->char x)))
               (apply irange (map char->integer (list x y)))))))
    (char-range #\a #\m)))

(define (find-clef gabc-str)
  (let ((match (string-match "\\(([cf])([1-4])" gabc-str)))
    (list
     `(type . ,(match:substring match 1))
     `(line . ,(string->number (match:substring match 2)))
     '(b . #f))))

(define (parse gabc-str)
  (let*
      ((header-delimiter "\n%%\n")
       (delimiter-position (string-contains gabc-str header-delimiter))
       (headerless
        (if delimiter-position
            (substring gabc-str (+ delimiter-position (string-length header-delimiter)))
            gabc-str)))
    (map
     (lambda (x)
       (parse-music-syllable
        (string-trim-both (match:substring x 1))
        (match:substring x 2)))
     (list-matches "([^\\(]*)\\(([^\\)]*)\\)" headerless))))

(define (parse-music-syllable lyrics music)
  (let ((matches (list-matches "(([cf])([1-4])|([a-m])|([,;:]+))" music)))
    (filter
     (lambda (x) (not (eq? #f x)))
     (append
      (list
       (if (> (string-length lyrics) 0)
           (list 'lyrics lyrics)
           #f))
      (map
       (lambda (m)
         (cond
          ((match:substring m 3)
           (list 'clef (match:substring m 2) (string->number (match:substring m 3)) #f))
          ((match:substring m 5)
           (list 'divisio (match:substring m 5)))
          (else
           (list 'note (match:substring m 4)))))
       matches)))))
