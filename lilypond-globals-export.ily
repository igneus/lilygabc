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

   (module-define! mdl 'open-episema
    (define-music-function (note) (ly:music?)
     #{ #note \episemInitium #}))

   (module-define! mdl 'close-episema
    (define-music-function (note) (ly:music?)
     #{ #note \episemFinis #}))

   (module-define! mdl 'apply-virga
    (define-music-function (side note) (boolean-or-symbol? ly:music?)
     (define (stems-hidden? context)
      (let* ((grob-def (ly:context-grob-definition context 'Stem))
             (length (ly:assoc-get 'length grob-def)))
       ;; see \lilygabcModernGregorianStemlessLayout
       (and (number? length) (= 0 length))))

     (define stems-were-hidden #f)

     (make-sequential-music
      (list
       (make-apply-context
        (lambda (context)
         (when (stems-hidden? context)
          (set! stems-were-hidden #t)
          (ly:context-pushpop-property context 'Stem 'length)
          (ly:context-pushpop-property context 'Stem 'direction DOWN)
          (when (eq? 'right side)
           (ly:context-pushpop-property context 'NoteHead 'stem-attachment '(0.8 . 0.3))))))

       note

       (make-apply-context
        (lambda (context)
         (when stems-were-hidden
          (ly:context-pushpop-property context 'Stem 'length 0)
          (ly:context-pushpop-property context 'Stem 'direction)
          (when (eq? 'right side)
           (ly:context-pushpop-property context 'NoteHead 'stem-attachment)))))
     ))))

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
