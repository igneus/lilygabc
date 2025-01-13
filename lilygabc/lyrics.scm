;; processing lyrics

(define-module (lilygabc lyrics)
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

;; main entry point
(define-public (expand str)
  (process-formatting (remove-braces (expand-special-chars str))))

(define-public (expand-special-chars str)
  (regexp-substitute/global
   #f sp-tag-re str
   'pre
   (lambda (m)
     (or (assoc-ref special-chars (match:substring m 1))
         (match:substring m 0)))
   'post))

(define-public formatting-tags
  '(("b"  . bold)
    ("i"  . italic)
    ("sc" . smallCaps)
    ("u"  . underline)
    ("c"  . color)))

(define tag-re (make-regexp "<(/)?([^>]+)>"))

(define (formatting-inner result str active-formats)
  (let ((m (regexp-exec tag-re str)))
    (if m
        (formatting-inner
         (let ((pre-str (match:prefix m)))
           (if (string-null? pre-str)
               result
               (append result (list (apply-formats active-formats pre-str)))))
         (match:suffix m)
         (update-active-formats (not (match:substring m 1)) (match:substring m 2) active-formats))
        (if (string-null? str)
            result
            (append result (list str))))))

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
  (fold-right (cut list <> <>) str formats))

(define-public (process-formatting str)
  (formatting-inner '() str '()))

(define-public (remove-braces str)
  (string-delete
   (cut member <> '(#\{ #\}))
   str))
