;; LilyPond music functions and expressions that are available
;; in the global namespace of any LilyPond document,
;; but are not imported as part of the (lily) module.
;; This module is filled with actual content in lilypond-globals-export.ily.

(define-module (lilygabc lily lilypond-globals)
  #:export
  ;; fundamental LilyPond syntax features
  (bar
   break
   breathe
   clef
   hideNotes
   once
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
   virga))
