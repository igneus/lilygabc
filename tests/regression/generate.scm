;; Accepts a list of gabc file paths,
;; prints to stdout a LilyPond document
;; which dumps their LilyPond and Scheme data structures.

(use-modules
 (ice-9 match))

(define (for-each2 list fn) (for-each fn list))

(define (generate-tests styles structures filenames)
  (for-each2
   filenames
   (lambda (f)
     (for-each2
      styles
      (lambda (style)
        (for-each2
         structures
         (lambda (stru)
           (item f style stru))))))))

(define (item filename style structure)
  (match-let
      (((structure-label . display-fn) structure)
       ((style-label . render-fn) style))
    (display (string-append "#(display \"% test " filename ": " style-label ": " structure-label "\\n\")\n"))
    (display (string-append "\\void \\" display-fn " \\" render-fn " \"" filename "\"\n"))))



(for-each
 display
 '("\\version \"2.24.0\"\n"
   "\\include \"gregorian.ly\"\n"
   "\\include \"../../lilygabc.ily\"\n\n"))

(generate-tests
 '(("modern notation" . "gabc-file")
   ("square notation" . "gabc-vaticana-file"))
 '(("LilyPond" . "displayLilyMusic")
   ("Scheme"   . "displayMusic"))
 (cdr (command-line)))
