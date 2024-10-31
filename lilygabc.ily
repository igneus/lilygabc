#(add-to-load-path (dirname (current-filename)))
#(load "lilygabc.scm")

\include "gregorian-shim.ily"

%
% custom articulation marks
%

#(define lilygabc:modern-gregorian:ictus-stencil
  (make-line-stencil 0.1 0 0 0 0.7))

#(define lilygabc-modern-gregorian-script-alist
  (util:alist-merge
   default-script-alist
   `((staccatissimo
      . ((stencil . ,lilygabc:modern-gregorian:ictus-stencil)
         (toward-stem-shift-in-column . 0.0)
         (padding . 0.4)
         (avoid-slur . around)
         (direction . ,DOWN))))))



%
% layout variables
% providing decent looks for lilygabc scores
%

lilygabcModernGregorianLayout = \layout {
  \set Timing.timing = ##f

  \context {
    \Score
    scriptDefinitions = #lilygabc-modern-gregorian-script-alist
  }
  \context {
    \Staff
    \remove Time_signature_engraver
  }
}

lilygabcModernGregorianStemlessLayout = \layout {
  \lilygabcModernGregorianLayout

  \override Stem.length = 0
}

% Nota bene: if you plan to use this layout variable,
% order in which lilygabc.ily and gregorian.ly are \included
% has serious consequences for the contents of the variable,
% as described in http://lilypond.org/doc/v2.24/Documentation/notation/the-layout-block
% The strongly suggested order is to include gregorian.ly first.
lilygabcVaticanaLayout = \layout {
  % staff and lyrics font size to match the modern notation
  % when included in the same document
  #(layout-set-staff-size 26)

  % default value of -4 is set in engraver-init.ly
  \override VaticanaLyrics.LyricText.font-size = #-0.8
}



% Internal utility music functions used by the Scheme modules.
#(begin
  (let ((mdl (resolve-module '(lilygabc lily music-functions))))
   (module-define! mdl 'make-invisible-note
    (define-music-function () () #{ \once \hideNotes g'4 #}))

   ;; N.B.: all functions applying features to a note
   ;; must have that note as their last expression
   ;; (not e.g. a state-resetting command).
   ;; Otherwise a subsequent application of another function
   ;; on the result of the function call
   ;; results in an unattached ArticulationEvent.
   (module-define! mdl 'tiny-note
    (define-music-function (note) (ly:music?)
     #{ \once \tiny #note #}))

   (module-define! mdl 'teeny-note
    (define-music-function (note) (ly:music?)
     #{ \once \teeny #note #}))

   (module-define! mdl 'apply-articulation
    (define-music-function (articulation note) (ly:music? ly:music?)
     #{ #note #articulation #}))

   (module-define! mdl 'apply-articulation-down
    (define-music-function (articulation note) (ly:music? ly:music?)
     #{ #note _#articulation #}))

   (module-define! mdl 'apply-articulation-up
    (define-music-function (articulation note) (ly:music? ly:music?)
     #{ #note ^#articulation #}))

   (module-define! mdl 'apply-single-note-episema
    (define-music-function (note) (ly:music?)
     #{ #note \episemInitium \episemFinis #}))

   (module-define! mdl 'open-episema
    (define-music-function (note) (ly:music?)
     #{ #note \episemInitium #}))

   (module-define! mdl 'close-episema
    (define-music-function (note) (ly:music?)
     #{ #note \episemFinis #}))

   (module-define! mdl 'apply-virga
    (define-music-function (side note) (boolean-or-symbol? ly:music?)
     ; TODO it would be safer to check first that Stem.length is actually overridden to 0
     #{
       \once \stemDown
       \once \revert Stem.length
       #(if (eq? 'right side)
         #{ \once \override NoteHead.stem-attachment = #'(0.8 . 0.3) #})
       #note
       #}))

   (module-define! mdl 'apply-musica-ficta
    (define-music-function (note) (ly:music?)
     #{ \once \set suggestAccidentals = ##t #note #}))

   (module-define! mdl 'key-flat #{ { \key f \major } #})
   (module-define! mdl 'key-natural #{ { \key c \major } #})

   ;; standard LilyPond names which aren't available by importing the (lily) module
   (for-each
    (lambda (sym)
     (module-define! mdl sym (eval sym (current-module))))
    '(bar
      break
      breathe
      clef
      hideNotes
      once
      prall
      staccatissimo
      teeny
      tenuto

      lilygabcAccentGrave
      lilygabcSemicircleUpper))

   ;; gregorian.ly names
   (when (defined? 'virga) ; gregorian.ly is \included
    (for-each
     (lambda (sym)
      (module-define! mdl sym (eval sym (current-module))))
     '(accentus
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
       virga)))))
