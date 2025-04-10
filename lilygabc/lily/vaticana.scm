(define-module (lilygabc lily vaticana)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (lily) ; LilyPond Scheme API
  #:use-module ((lilygabc episema) #:prefix episema:)
  #:use-module ((lilygabc gabc) #:prefix gabc:)
  #:use-module (lilygabc ligature)
  #:use-module ((lilygabc pitch) #:prefix pitch:)
  #:use-module ((lilygabc util) #:prefix util:)
  #:use-module ((lilygabc lily lilypond-globals) #:prefix l:)
  #:use-module (lilygabc lily music-functions)
  #:use-module ((lilygabc lily modern) #:select (make-ly-note apply-episema-events expand-note-repetitions syl-has-decorated-notes? unrecognized-element-warning)))

;; mapping Gregorio divisiones -> gregorian.ly bars
;;
;; TODO how comes these are defined here? Are they somehow included
;; in the (lily) module, although they are defined in LilyPond
;; (not Scheme) code?
(define divisiones-mapping
  '(("," . divisioMinima)
    (";" . divisioMaior)
    ("::" . finalis)
    ("`" . virgula)))
(define default-bar 'divisioMaxima) ; used for all not explicitly mapped

(define-public (make-notes score)
  (let*
      ((enhanced-score
        (pitch:decorate-notes
         (gabc:map-syllables
          (lambda (syl) (add-ligatures (expand-note-repetitions syl)))
          score)
         #:base-octave -1)) ; LilyPond treats the chant c clef as middle c
       (syllables (util:flatten enhanced-score)))
    (make-sequential-music
     (append-map
      (lambda (syllable)
        (let* ((notes (filter pitch:is-note-with-pitch? syllable))
               (is-melisma (< 1 (length notes)))
               (syllable-properties
                `((is-melisma . ,is-melisma)
                  (first-note . ,(and is-melisma (first notes)))
                  (last-note . ,(and is-melisma (last notes)))))
               (items-with-episema-events
                (episema:decorate-notes
                 pitch:is-note-with-pitch?
                 (lambda (x) (gabc:note-has-horizontal-episema? (second x)))
                 syllable))
               (previous-note #f)) ;; mutable state
          (cond
           ((eq? '() syllable) ; void syllable rendered as invisible bar
            (list (l:bar "")))
           ((and (gabc:syl-has-lyrics? syllable)
                 (= 1 (length syllable))) ; the syllable has only lyrics, no renderable music elements
            (list (make-invisible-note)))
           (else
            (util:flatten
             (util:map-with-previous
              (lambda (previous-item item)
                (match item
                  (('clef type line clef-is-flat)
                   (make-clef type line clef-is-flat))
                  (('note-with-episema-events ('note-with-pitch note pitch) episema-events)
                   (let ((n (make-note note pitch episema-events syllable-properties previous-note item previous-item)))
                     (set! previous-note (second item))
                     n))
                  (('divisio type)
                   (make-divisio type syllable))
                  (('space type)
                   (make-neumatic-space type previous-note))
                  (('line-break type)
                   (list l:break))
                  (('ligature event)
                   (when (eq? 'open event)
                     (set! previous-note #f))
                   (list (make-music
                          'LigatureEvent
                          'span-direction (if (eq? 'open event) -1 1))))
                  (('unrecognized token)
                   (unrecognized-element-warning token (*location*))
                   '())
                  (any '())))
              items-with-episema-events))))))
      syllables))))

(define (make-clef type line is-flat)
  (let* ((lily-clefnum (- line 1)))
    (append
     (list (l:clef (string-append
                    "vaticana-"
                    (if (string=? "f" type) "fa" "do")
                    (number->string lily-clefnum))))
     (if is-flat (list key-flat) '()))))

(define (make-note note pitch episema-events syllable-props previous-note item previous-item)
  (let ((is-melisma (assoc-ref syllable-props 'is-melisma)) ; TODO find a shortcut for this
        (first-note (assoc-ref syllable-props 'first-note))
        (last-note (assoc-ref syllable-props 'last-note)))
    (append
     (cond
      ((and is-melisma
            previous-note
            (not (or (pitch:pitch=? pitch (third previous-note))
                     (gabc:note-is-punctum-inclinatum? note)
                     (gabc:note-is-virga? note)
                     (gabc:note-is-punctum-inclinatum? (second previous-note))
                     (gabc:note-is-virga? (second previous-note))
                     (and previous-item (eq? 'space (first previous-item))))))
       (list
        (l:once
         (context-spec-music
          (make-grob-property-override 'NoteHead 'pes-or-flexa #t)
          'Bottom))))
      (else
       '()))
     (apply-note-features-2
      note
      (list
       (apply-note-features-1
        note
        (apply-episema-events
         episema-events
         (make-ly-note
          (apply ly:make-pitch (list-tail pitch 1))
          (ly:make-duration 2)
          #f)))))
     (cond
      ((and is-melisma (eq? first-note (second item)))
       (list (context-spec-music (make-property-set 'melismaBusy #t) 'Bottom)))
      ((and is-melisma (eq? last-note (second item)))
       (list (context-spec-music (make-property-unset 'melismaBusy) 'Bottom)))
      (else '())))))

;; apply features that are music functions
(define (apply-note-features-1 gabc-note ly-note)
  (let ((tests-and-transformations
         `((,gabc:note-has-punctum-mora? . ,l:augmentum)
           (,(lambda (x) (< 1 (gabc:note-punctum-mora-count x))) . ,l:augmentum)
           (,gabc:note-has-ictus? . ,(cut apply-articulation l:ictus <>))
           (,gabc:note-has-accentus? . ,(cut apply-articulation l:accentus <>))
           (,gabc:note-has-accent-grave? . ,(cut apply-articulation lilygabcAccentGrave <>))
           (,gabc:note-has-circulus? . ,(cut apply-articulation l:circulus <>))
           (,gabc:note-has-semicirculus? . ,(cut apply-articulation l:semicirculus <>))
           (,gabc:note-has-semicirculus-upper? . ,(cut apply-articulation lilygabcSemicircleUpper <>))
           (,gabc:note-has-musica-ficta? . ,apply-musica-ficta))))
    (fold
     (lambda (x r)
       (match-let (((test . transformation) x))
         (if (test gabc-note)
             (transformation r)
             r)))
     ly-note
     tests-and-transformations)))

;; apply features that are prepended music elements
(define (apply-note-features-2 gabc-note ly-note-list)
  (let ((tests-and-transformations
         `((,gabc:note-is-diminutive? . ,l:deminutum)
           (,gabc:note-is-punctum-inclinatum? . ,l:inclinatum)
           (,gabc:note-is-debilis? . ,l:deminutum)
           (,gabc:note-is-virga? . ,l:virga)
           (,(lambda (x)
               (and (gabc:note-is-ascendens? x)
                    (not (gabc:note-is-stropha? x))))
            . ,l:ascendens)
           (,gabc:note-is-ascendens? . ,l:auctum)
           (,gabc:note-is-descendens? . ,l:descendens)
           (,gabc:note-is-descendens? . ,l:auctum)
           (,gabc:note-is-quilisma? . ,l:quilisma)
           (,gabc:note-is-oriscus? . ,l:oriscus)
           (,gabc:note-is-cavum? . ,l:cavum)
           (,gabc:note-has-linea? . ,l:linea)
           (,gabc:note-is-stropha? . ,l:stropha))))
    (fold
     (lambda (x r)
       (match-let (((test . transformation) x))
         (if (test gabc-note)
             (append (list transformation) r)
             r)))
     ly-note-list
     tests-and-transformations)))

(define (make-divisio type syllable)
  (filter
   values
   (list
    (if (and (gabc:syl-has-lyrics? syllable)
             (not (syl-has-decorated-notes? syllable)))
        ;; lyrics under a divisio are very common in gabc,
        ;; but unsupported in LilyPond
        (make-invisible-note)
        #f)
    (primitive-eval (or (assoc-ref divisiones-mapping type)
                        default-bar)))))

(define (make-neumatic-space type previous-note)
  (let* ((previous-pitch
          (if previous-note
              (apply ly:make-pitch (list-tail (third previous-note) 1))
              (ly:make-pitch -1 4)))
         (one-step-below (ly:pitch-transpose previous-pitch (ly:make-pitch -1 5 1/2)))) ; 1 diatonic step under the last note
    (cond
     ((string=? " " type)
      (invisible-note-on-pitch previous-pitch))
     ((string=? "//" type)
      (invisible-note-on-pitch one-step-below))
     ((string=? "/" type)
      (list
       (l:once l:hideNotes) (l:once l:teeny)
       (make-ly-note one-step-below (ly:make-duration 2) #f)))
     (else '()))))

(define (invisible-note-on-pitch pitch)
  (list
   (l:once l:hideNotes)
   (make-ly-note pitch (ly:make-duration 2) #f)))
