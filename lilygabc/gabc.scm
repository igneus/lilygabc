;; gabc parsing - pure Scheme, no LilyPond-specific types/functions

(define-module (lilygabc gabc)
  #:export (parse
            syl-has-lyrics?
            syl-has-notes?
            clef-has-bflat?
            note-is-punctum-inclinatum?
            note-is-diminutive?
            note-is-debilis?
            note-is-ascendens?
            note-is-descendens?
            note-is-oriscus?
            note-is-quilisma?
            note-is-cavum?
            note-is-stropha?
            note-is-virga?
            note-has-punctum-mora?
            note-has-ictus?
            note-has-horizontal-episema?
            note-has-linea?
            note-has-accentus?
            note-has-accent-grave?
            note-has-circulus?
            note-has-semicirculus?
            note-has-semicirculus-upper?
            note-has-musica-ficta?
            note-virga-side
            note-repetitions
            note-punctum-mora-count
            note-ficta-value
            note-has-special-note-head?
            map-syl-elements)
  #:use-module (srfi srfi-1) ; required to use the correct version of `iota`
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module ((lilygabc util) #:prefix util:))

(define header-delimiter "\n%%\n")

;; Parses gabc string and returns its tree-like representation.
;; Score is a list of words.
;; Word is a list of syllables.
;; Syllable is a list of syllable elements.
;;   Lyrics, if present, are always the first element of a syllable.
;; Syllable element is a list where the first item is a symbol
;;   specifying element type, subsequent items vary depending on type.
(define (parse gabc-str)
  (let*
      ((delimiter-position (string-contains gabc-str header-delimiter))
       (gabc-body
        (if delimiter-position
            (substring gabc-str (+ delimiter-position (string-length header-delimiter)))
            gabc-str))
       (syllables (list-matches "([^\\(]*)\\(([^\\)]*)\\)" gabc-body))
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
        (string-trim-both (remove-tags (match:substring x 1)))
        (match:substring x 2)))
     words)))

;; syllable predicates

(define (syl-has-lyrics? syllable)
  (find (lambda (x) (eq? 'lyrics (first x))) syllable))

(define (syl-has-notes? syllable)
  (find (lambda (x) (eq? 'note (first x))) syllable))

;; clef predicates

(define (clef-has-bflat? clef)
  (fourth clef))

;; note predicates

(define (note-is-punctum-inclinatum? note)
  (char-upper-case? (string-ref (note-name note) 0)))

(define note-name second)

(define (note-additional note)
  (if (>= (length note) 3)
      (third note)
      ""))

(define (note-additional-contains? what note)
  (if (string? what)
      (string-contains (note-additional note) what)
      (string-index (note-additional note) what)))

(define (note-additional-matches? regexp note)
  (not (eq? #f (string-match regexp (note-additional note)))))

(define (note-is-diminutive? note)
  (string-prefix? "~" (note-additional note)))

(define (note-is-debilis? note)
  (string-suffix? "-" (note-additional note)))

(define (note-is-ascendens? note)
  (note-additional-contains? #\< note))

(define (note-is-descendens? note)
  (note-additional-contains? #\> note))

(define (note-is-oriscus? note)
  (note-additional-contains? #\o note))

(define (note-is-quilisma? note)
  (note-additional-contains? #\w note))

(define (note-is-cavum? note)
  (note-additional-matches? "r($|[^1-9])" note))

(define (note-is-stropha? note)
  (note-additional-contains? #\s note))

(define (note-is-virga? note)
  (not (eq? #f (note-virga-side note))))

(define (note-has-punctum-mora? note)
  (note-additional-contains? #\. note))

(define (note-has-ictus? note)
  (note-additional-contains? #\' note))

(define (note-has-horizontal-episema? note)
  (note-additional-contains? #\_ note))

(define (note-has-linea? note)
  (note-additional-matches? "R|r0" note))

(define (note-has-accentus? note)
  (note-additional-contains? "r1" note))

;; TODO is there a standard Latin name?
(define (note-has-accent-grave? note)
  (note-additional-contains? "r2" note))

(define (note-has-circulus? note)
  (note-additional-contains? "r3" note))

(define (note-has-semicirculus? note)
  (note-additional-contains? "r4" note))

;; TODO is there a standard Latin name?
(define (note-has-semicirculus-upper? note)
  (note-additional-contains? "r5" note))

(define (note-has-musica-ficta? note)
  (note-additional-matches? "r[6-8]" note))

(define (note-virga-side note)
  (let ((a (note-additional note)))
    (cond
     ((string-index a #\V) 'left)
     ((string-index a #\v) 'right)
     (else #f))))

(define (note-repetitions note)
  (let ((m (string-match "[vs]{2,3}" (note-additional note))))
    (and m (string-length (match:substring m 0)))))

(define (note-punctum-mora-count note)
  (let ((m (string-match "\\.{1,2}" (note-additional note))))
    (or (and m (string-length (match:substring m 0)))
        0)))

(define (note-ficta-value note)
  (let* ((m (string-match "r([6-8])" (note-additional note)))
         (num (and m (string->number (match:substring m 1)))))
    (/ (- num 7) 2))) ; LOL

;; any note head other than a simple punctum
(define (note-has-special-note-head? note)
  (or (char-upper-case? (string-ref (note-name note) 0))
      (string-match "[-~<>=osvVwWR]|r($|[^1-9])" (note-additional note))))

;; operations on the score data structure

(define map-syl-elements util:map3)

;; maps gabc accidentals to symbols we return in the parsing results
(define accidentals
  '(("x" . flat)
    ("#" . sharp)
    ("y" . natural)))

(define (parse-music-syllable lyrics music)
  (let ((matches (list-matches "([cf]b?[1-4])|([a-m][xy#])|(-)?([a-mA-M])([n-zN-Z~<>\\._'0-9]+)?|([,;:`]+)|([!@ ]|/{1,2})|\\|(.*$)" music)))
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
               (note-debilis (match:substring m 3))
               (note (match:substring m 4))
               (note-shape (match:substring m 5))
               (divisio (match:substring m 6))
               (space (match:substring m 7))
               (nabc (match:substring m 8)))
           (cond
            (clef
             (list 'clef
                   (substring clef 0 1)
                   (string->number (substring (string-reverse clef) 0 1))
                   (> (string-length clef) 2)))
            (accidental
             (list 'accidental
                   (substring accidental 0 1)
                   (assoc-ref accidentals (substring accidental 1 2))))
            (divisio
             (list 'divisio divisio))
            (space
             (list 'space space))
            (nabc
             (list 'nabc nabc))
            (else
             (append
              (list 'note note)
              (if (or note-shape note-debilis)
                  ;; TODO: check if it is safe to append the "-"
                  ;;   of initium debilis to the end of the shape
                  ;;   specifying characters or if it also has other
                  ;;   uses beyond in. deb. and must be handled differently
                  ;;   (probably by redesigning the parsed representation
                  ;;   of note features)
                  (list (string-join (filter values (list note-shape note-debilis)) ""))
                  '()))))))
       matches)))))

(define (remove-tags lyric-syllable)
  (regexp-substitute/global #f "<[^>]*>" lyric-syllable 'pre "" 'post))
