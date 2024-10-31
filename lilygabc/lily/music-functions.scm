;; Music functions and music expressions
;; defined in LilyPond code. In lilygabc.ily they are
;; added to this module via module reflection
;; in order to make them available to Scheme modules.

(define-module (lilygabc lily music-functions)
  #:export
  ;; utilities defined in lilygabc.ily
  (make-invisible-note
   tiny-note
   teeny-note
   apply-musica-ficta
   apply-articulation
   apply-articulation-up
   apply-articulation-down
   apply-virga
   apply-single-note-episema
   key-flat
   key-natural

   ;; fundamental LilyPond syntax features
   bar
   break
   breathe
   clef
   close-episema
   hideNotes
   once
   open-episema
   prall
   staccatissimo
   teeny
   tenuto

   ;; gregorian.ly variables
   accentus
   ascendens
   auctum
   augmentum
   cavum
   circulus
   deminutum
   descendens
   ictus
   inclinatum
   linea
   oriscus
   quilisma
   semicirculus
   stropha
   virga

   ;; gregorian-shim.ly variables
   lilygabcAccentGrave
   lilygabcSemicircleUpper))
