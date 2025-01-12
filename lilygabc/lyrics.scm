;; processing lyrics

(define-module (lilygabc lyrics)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26))

(define sp-tag-re (make-regexp "<sp>([^<]*)</sp>"))

;; List of the <sp> values supported by default by Gregorio is in
;; the official documentation; the actual code is in
;; https://github.com/gregorio-project/gregorio/blob/master/tex/gregoriotex-symbols.tex
;; - search for a series of \gresetspecial command invocations.
;; We only handle a subset.
(define special-chars
  '(("R/" . "℟")
    ("V/" . "℣")
    ("ae" . "æ")
    ("'ae" . "ǽ")
    ("'æ" . "ǽ")
    ("AE" . "Æ")
    ("oe" . "œ")
    ("'oe" . "œ́")
    ("'œ" . "œ́")
    ("OE" . "Œ")
    ("+" . "†")))

(define-public (expand-special-chars str)
  (regexp-substitute/global
   #f sp-tag-re str
   'pre
   (lambda (m)
     (or (assoc-ref special-chars (match:substring m 1))
         (match:substring m 0)))
   'post))

(define-public (remove-tags str)
  (regexp-substitute/global #f "<[^>]*>" str 'pre "" 'post))

(define-public (remove-braces str)
  (string-delete
   (cut member <> '(#\{ #\}))
   str))
