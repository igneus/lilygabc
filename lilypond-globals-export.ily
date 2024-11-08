%% This makes some functions and variables defined in LilyPond code
%% available to Scheme modules.

#(begin

  ;; Internal utility music functions used by the Scheme modules.
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

   (module-define! mdl 'key-flat #{ \key f \major #})
   (module-define! mdl 'key-natural #{ \key c \major #})

   (for-each
    (lambda (sym)
     (module-define! mdl sym (eval sym (current-module))))
    '(lilygabcAccentGrave
      lilygabcSemicircleUpper)))

  ;; standard LilyPond names which aren't available by importing the (lily) module
  (let ((mdl (resolve-module '(lilygabc lily lilypond-globals))))
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
      tenuto))

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
