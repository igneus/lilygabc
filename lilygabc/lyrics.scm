;; processing lyrics

(define-module (lilygabc lyrics)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define sp-tag-re (make-regexp "<sp>([^<]*)</sp>"))

;; List of the <sp> values supported by default by Gregorio is in
;; the official documentation; the actual code is in
;; https://github.com/gregorio-project/gregorio/blob/master/tex/gregoriotex-symbols.tex
;; - search for a series of \gresetspecial command invocations.
;; We only handle a subset.
(define special-chars
  '(("R/"  . "℟")
    ("V/"  . "℣")
    ("ae"  . "æ")
    ("'ae" . "ǽ")
    ("'æ"  . "ǽ")
    ("AE"  . "Æ")
    ("oe"  . "œ")
    ("'oe" . "œ́")
    ("'œ"  . "œ́")
    ("OE"  . "Œ")
    ("+"   . "†")))

;; Main entry point.
;; Returns a function which accepts a lyric syllable as string
;; and returns it expanded.
;; The function is not thread-safe, must be used on syllables
;; in order and only for a single score, as it's stateful
;; and keeps track of formatting tags spanning multiple syllables.
(define*-public (expander #:key (custom-special-chars '()))
  (let ((active-formats '()))
    (lambda (str)
      (let
          ((r (expand-formatting
               '()
               (remove-braces
                (expand-special-chars str #:custom custom-special-chars))
               active-formats)))
        (set! active-formats (car r))
        (cdr r)))))

(define*-public (expand-special-chars str #:key (custom '()))
  (let ((mapping (append custom special-chars)))
    (regexp-substitute/global
     #f sp-tag-re str
     'pre
     (lambda (m)
       (or (assoc-ref mapping (match:substring m 1))
           (match:substring m 0)))
     'post)))

(define-public formatting-tags
  '(("b"  . bold)
    ("i"  . italic)
    ("sc" . smallCaps)
    ("ul" . underline)
    ("tt" . typewriter)
    ("c"  . color)
    ("v"  . verbatim)))

(define tag-re (make-regexp "<(/)?([^>]+)>"))

(define-public (expand-formatting result str active-formats)
  (let ((m (regexp-exec tag-re str)))
    (if m
        (expand-formatting
         (let ((pre-str (match:prefix m)))
           (if (string-null? pre-str)
               result
               (append result (list (apply-formats active-formats pre-str)))))
         (match:suffix m)
         (update-active-formats (not (match:substring m 1)) (match:substring m 2) active-formats))
        (cons
         active-formats
         (if (string-null? str)
             result
             (append result (list (apply-formats active-formats str))))))))

(define (update-active-formats is-addition tag-name active-formats)
  (let* ((format-sym (assoc-ref formatting-tags tag-name))
         (is-active (member format-sym active-formats)))
    (if (not format-sym)
        active-formats
        (if is-addition
            (if is-active
                active-formats
                (append active-formats (list format-sym)))
            (if (not is-active)
                active-formats
                (remove (cut equal? format-sym <>) active-formats))))))

(define (apply-formats formats str)
  (if (null? formats)
      str
      (append formats (list str))))

;; removes curly braces except those inside <v></v> tags
(define*-public (remove-braces str #:optional (in-verbatim #f))
  (let* ((tag (if in-verbatim "</v>" "<v>"))
         (tag-i (string-contains str tag))
         (subject
          (if tag-i
              (substring str 0 (+ tag-i (string-length tag)))
              str))
         (processed
          (if in-verbatim
              subject
              (string-delete
               (cut member <> '(#\{ #\}))
               subject))))
    (if tag-i
        (string-append
         processed
         (remove-braces
          (substring str (string-length subject))
          (not in-verbatim)))
        processed)))
