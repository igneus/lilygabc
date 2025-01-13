;; Music functions and music expressions
;; defined in LilyPond code. They are
;; added to this module via module reflection
;; in order to make them available to Scheme modules.

(define-module (lilygabc lily music-functions)
  #:export
  ;; utilities defined in lilypond-globals-export.ily
  (make-invisible-note
   tiny-note
   teeny-note
   apply-musica-ficta
   apply-articulation
   apply-articulation-up
   apply-articulation-down
   open-episema
   close-episema
   apply-virga
   key-flat
   key-natural

   ;; gregorian-shim.ly variables
   lilygabcAccentGrave
   lilygabcSemicircleUpper))
