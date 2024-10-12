;; gabc parsing - pure Scheme, no LilyPond-specific types/functions

(define-module (lilygabc gabc)
  #:export (parse
            syl-has-lyrics?
            syl-has-notes?
            note-is-punctum-inclinatum?
            note-is-diminutive?
            note-has-punctum-mora?
            note-has-ictus?
            note-has-horizontal-episema?
            note-virga-side
            map-syl-elements)
  #:use-module (srfi srfi-1) ; required to use the correct version of `iota`
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module ((lilygabc util) #:prefix util:))

;; Parses gabc string and returns its tree-like representation.
;; Score is a list of words.
;; Word is a list of syllables.
;; Syllable is a list of syllable elements.
;;   Lyrics, if present, are always the first element of a syllable.
;; Syllable element is a list where the first item is a symbol
;;   specifying element type, subsequent items vary depending on type.
(define (parse gabc-str)
  (let*
      ((header-delimiter "\n%%\n")
       (delimiter-position (string-contains gabc-str header-delimiter))
       (headerless
        (if delimiter-position
            (substring gabc-str (+ delimiter-position (string-length header-delimiter)))
            gabc-str))
       (syllables (list-matches "([^\\(]*)\\(([^\\)]*)\\)" headerless))
       (words
        (util:split-at
         (lambda (x)
           (let ((str (match:substring x 0)))
             (or (string-prefix? " " str)
                 (string-prefix? "\n" str))))
         syllables)))
    (util:map2
     (lambda (x)
       (parse-music-syllable
        (string-trim-both (match:substring x 1))
        (match:substring x 2)))
     words)))

;; syllable predicates

(define (syl-has-lyrics? syllable)
  (find (lambda (x) (eq? 'lyrics (first x))) syllable))

(define (syl-has-notes? syllable)
  (find (lambda (x) (eq? 'note (first x))) syllable))

(define (note-is-punctum-inclinatum? note)
  (char-upper-case? (string-ref (second note) 0)))

;; note predicates

(define (note-additional note)
  (if (>= (length note) 3)
      (third note)
      ""))

(define (note-is-diminutive? note)
  (string-prefix? "~" (note-additional note)))

(define (note-has-punctum-mora? note)
  (string-index (note-additional note) #\.))

(define (note-has-ictus? note)
  (string-index (note-additional note) #\'))

(define (note-has-horizontal-episema? note)
  (string-index (note-additional note) #\_))

(define (note-virga-side note)
  (let ((a (note-additional note)))
    (cond
     ((string-index a #\V) 'left)
     ((string-index a #\v) 'right)
     (else #f))))

;; operations on the score data structure

(define map-syl-elements util:map3)

;; maps gabc accidentals to symbols we return in the parsing results
(define accidentals
  '(("x" . flat)
    ("#" . sharp)
    ("y" . natural)))

(define (parse-music-syllable lyrics music)
  (let ((matches (list-matches "([cf][1-4])|([a-m][xy#])|([a-mA-M])([n-zN-Z~<>\\._']+)?|([,;:`]+)|\\|(.*$)" music)))
    (filter
     values
     (append
      (list
       (if (> (string-length lyrics) 0)
           (list 'lyrics lyrics)
           #f))
      (map
       (lambda (m)
         (let ((clef (match:substring m 1))
               (accidental (match:substring m 2))
               (note (match:substring m 3))
               (note-shape (match:substring m 4))
               (divisio (match:substring m 5))
               (nabc (match:substring m 6)))
           (cond
            (clef
             (list 'clef
                   (substring clef 0 1)
                   (string->number (substring clef 1 2)) #f))
            (accidental
             (list 'accidental
                   (substring accidental 0 1)
                   (assoc-ref accidentals (substring accidental 1 2))))
            (divisio
             (list 'divisio divisio))
            (nabc
             (list 'nabc nabc))
            (else
             (append
              (list 'note note)
              (if note-shape (list note-shape) '()))))))
       matches)))))
