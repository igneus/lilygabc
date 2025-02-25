;; gabc parsing - pure Scheme, no LilyPond-specific types/functions

(define-module (lilygabc gabc)
  #:use-module (srfi srfi-1) ; required to use the correct version of `iota`
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module ((lilygabc util) #:prefix util:))

(define header-delimiter-re (make-regexp "\n%%\r?\n"))

(define gabc-syllable-re (make-regexp "([^(]*)\\(([^)]*)\\)"))

;; Parses gabc string and returns its tree-like representation.
;; Score is a list of words.
;; Word is a list of syllables.
;; Syllable is a list of syllable elements.
;;   Lyrics, if present, are always the first element of a syllable.
;; Syllable element is a list where the first item is a symbol
;;   specifying element type, subsequent items vary depending on type.
(define-public (parse gabc-str)
  (let*
      ((gabc-body (without-comments (body gabc-str)))
       (syllables (list-matches gabc-syllable-re gabc-body))
       (words
        (util:split-at
         (lambda (x)
           (let ((str (match:substring x 0)))
             (or (string-prefix? " " str)
                 (string-prefix? "\n" str))))
         syllables)))
    (util:map2
     (lambda (x)
       (let ((lyrics (string-trim-both (match:substring x 1))))
         (append
          (if (string-null? lyrics)
              '()
              `((lyrics ,lyrics)))
          (parse-music-syllable (match:substring x 2)))))
     words)))

(define gly-syllable-re (make-regexp "([^[:space:]\\(]+)|\\(([^\\)]+)\\)"))

;; gly is a language reimagining the structure of gabc
;; (more at https://github.com/igneus/gly ).
;; This function doesn't parse the input as a complete
;; gly score or document, but as lines of gly music
;; (~ gabc with no lyrics and optional parentheses).
;; Typical use case is entering music as gly
;; and lyrics using LilyPond native syntax.
(define-public (parse-gly gly-str)
  (let
      ((syllables (list-matches gly-syllable-re gly-str)))
    (map
     (lambda (x)
       (list
        (parse-music-syllable
         (or (match:substring x 1)
             (match:substring x 2)))))
     syllables)))

(define (body gabc-str)
  (let* ((m (regexp-exec header-delimiter-re gabc-str))
         (delimiter-position (and m (match:end m))))
    (if delimiter-position
        (body (substring gabc-str delimiter-position))
        gabc-str)))

(define (without-comments gabc-str)
  (regexp-substitute/global #f (make-regexp "%[^$]*" regexp/newline) gabc-str 'pre "" 'post))

;; syllable predicates

(define-public (syl-has-lyrics? syllable)
  (find (lambda (x) (eq? 'lyrics (first x))) syllable))

(define-public (syl-has-notes? syllable)
  (find (lambda (x) (eq? 'note (first x))) syllable))

;; syllable item predicates

(define-public (is-note? item)
  (and (< 0 (length item))
       (eq? 'note (first item))))

;; clef predicates

(define-public (clef-has-bflat? clef)
  (fourth clef))

;; note predicates

(define-public (note-is-punctum-inclinatum? note)
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

(define-public (note-is-diminutive? note)
  (string-prefix? "~" (note-additional note)))

(define-public (note-is-debilis? note)
  (string-suffix? "-" (note-additional note)))

(define-public (note-is-ascendens? note)
  (note-additional-contains? #\< note))

(define-public (note-is-descendens? note)
  (note-additional-contains? #\> note))

(define-public (note-is-oriscus? note)
  (note-additional-contains? #\o note))

(define-public (note-is-quilisma? note)
  (note-additional-contains? #\w note))

(define-public (note-is-cavum? note)
  (note-additional-matches? "r($|[^1-9])" note))

(define-public (note-is-stropha? note)
  (note-additional-contains? #\s note))

(define-public (note-is-virga? note)
  (not (eq? #f (note-virga-side note))))

(define-public (note-has-punctum-mora? note)
  (note-additional-contains? #\. note))

(define-public (note-has-ictus? note)
  (note-additional-contains? #\' note))

(define-public (note-has-horizontal-episema? note)
  (note-additional-contains? #\_ note))

(define-public (note-has-linea? note)
  (note-additional-matches? "R|r0" note))

(define-public (note-has-accentus? note)
  (note-additional-contains? "r1" note))

;; TODO is there a standard Latin name?
(define-public (note-has-accent-grave? note)
  (note-additional-contains? "r2" note))

(define-public (note-has-circulus? note)
  (note-additional-contains? "r3" note))

(define-public (note-has-semicirculus? note)
  (note-additional-contains? "r4" note))

;; TODO is there a standard Latin name?
(define-public (note-has-semicirculus-upper? note)
  (note-additional-contains? "r5" note))

(define-public (note-has-musica-ficta? note)
  (note-additional-matches? "r[6-8]" note))

(define-public (note-virga-side note)
  (let ((a (note-additional note)))
    (cond
     ((string-index a #\V) 'left)
     ((string-index a #\v) 'right)
     (else #f))))

(define-public (note-repetitions note)
  (let ((m (string-match "[vs]{2,3}" (note-additional note))))
    (and m (string-length (match:substring m 0)))))

(define-public (note-punctum-mora-count note)
  (let ((m (string-match "\\.{1,2}" (note-additional note))))
    (or (and m (string-length (match:substring m 0)))
        0)))

(define-public (note-ficta-value note)
  (let* ((m (string-match "r([6-8])" (note-additional note)))
         (num (string->number (match:substring m 1))))
    (/ (- num 7) 2))) ; LOL

;; any note head other than a simple punctum
(define-public (note-has-special-note-head? note)
  (or (char-upper-case? (string-ref (note-name note) 0))
      (string-match "[-~<>=osvVwWR]|r($|[^1-9])" (note-additional note))))

;; operations on the score data structure

(define-public map-syllables util:map2)

(define-public map-syl-elements util:map3)

;; maps gabc accidentals to symbols we return in the parsing results
(define accidentals
  '(("x" . flat)
    ("#" . sharp)
    ("y" . natural)))

(define music-element-re (make-regexp "([cf]b?[1-4])|([a-m][xy#])|(-)?([a-mA-M])([n-yN-Y~<>\\._'0-9]+)?|([,;:`]+)|([!@ ]|/{1,2})|([zZ][+-]?)|\\[([^]]*)\\]|\\|(.*$)"))

(define (parse-music-syllable music)
  (let ((m (regexp-exec music-element-re music)))
    (if (not m)
        (if (string-null? music)
            '()
            `((unrecognized ,music)))
        (let* ((prefixx (match:prefix m))
               (suffixx (match:suffix m))
               (clef (match:substring m 1))
               (accidental (match:substring m 2))
               (note-debilis (match:substring m 3))
               (note (match:substring m 4))
               (note-shape (match:substring m 5))
               (divisio (match:substring m 6))
               (space (match:substring m 7))
               (line-break (match:substring m 8))
               (square-brackets (match:substring m 9))
               (nabc (match:substring m 10)))
          (append
           (if (not (string-null? prefixx))
               `((unrecognized ,prefixx))
               '())
           (list
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
             (line-break
              (list 'line-break line-break))
             (square-brackets
              (list 'square-brackets square-brackets))
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
                   '())))))
           (if (or (not suffixx) (string-null? suffixx))
               '()
               (parse-music-syllable suffixx)))))))
