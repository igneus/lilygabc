(use-modules
 (ice-9 match)
 (ice-9 regex)
 (ice-9 textual-ports)
 (srfi srfi-1)
 (srfi srfi-26)
 ((lilygabc episema) #:prefix episema:)
 ((lilygabc gabc) #:prefix gabc:)
 ((lilygabc pitch) #:prefix pitch:)
 ((lilygabc util) #:prefix util:))

; mapping Gregorio divisiones -> gregorian.ly bars
(define vaticana-divisiones-mapping
  '(("," . divisioMinima)
    (";" . divisioMaior)
    ("::" . finalis)
    ("`" . virgula)))
(define default-vaticana-bar 'divisioMaxima) ; used for all not explicitly mapped

(define (make-vaticana-notes score context-id)
  (let
      ((syllables (util:flatten score)))
    (context-spec-music
     (make-sequential-music
      (append-map
       (lambda (syllable)
         (let* ((notes (filter (lambda (x) (eq? 'note-with-pitch (first x))) syllable))
                (is-melisma (< 1 (length notes)))
                (first-note (and is-melisma (first notes)))
                (last-note (and is-melisma (last notes)))
                (is-single-note-special-head
                 (and (not is-melisma)
                      (< 0 (length notes))
                      (gabc:note-has-special-note-head? (second (first notes)))))
                (previous-note #f)
                (items-with-episema-events
                 (zip syllable
                      (episema:episema-events
                       (lambda (i) (and (eq? 'note-with-pitch (first i))
                                        (gabc:note-has-horizontal-episema? (second i))))
                       syllable))))
           (cond
            ((eq? '() syllable) ; void syllable rendered as invisible bar
             (list (bar "")))
            ((and (gabc:syl-has-lyrics? syllable)
                  (= 1 (length syllable))) ; the syllable has only lyrics, no renderable music elements
             (list (make-invisible-note)))
            (else
             (util:flatten
              (filter
               values
               (util:map-with-previous
                (lambda (previous current)
                  (let ((previous-item (and previous (first previous)))
                        (item (first current))
                        (episema-events (second current)))
                    (match item
                      (('clef type line clef-is-flat)
                       (let* ((lily-clefnum (- line 1)))
                         (append
                          (list (clef (string-append
                                       "vaticana-"
                                       (if (string=? "f" type) "fa" "do")
                                       (number->string lily-clefnum))))
                          (if clef-is-flat (list key-flat) '()))))
                      (('note-with-pitch note pitch)
                       ;; shift the pitch an octave lower:
                       ;; LilyPond treats the chant c clef as denoting middle c
                       (let ((vaticana-pitch (cons (- (second pitch) 1) (list-tail pitch 2))))
                         (append
                          (cond
                           ((or (and is-melisma (eq? first-note item))
                                is-single-note-special-head)
                            (list (make-music 'LigatureEvent 'span-direction -1)))
                           ((and is-melisma
                                 (not (or (pitch:pitch=? pitch (third previous-note))
                                          (gabc:note-is-punctum-inclinatum? note)
                                          (gabc:note-is-virga? note)
                                          (gabc:note-is-punctum-inclinatum? (second previous-note))
                                          (gabc:note-is-virga? (second previous-note))
                                          (and previous-item (eq? 'space (first previous-item))))))
                            (list
                             (once
                              (context-spec-music
                               (make-grob-property-override 'NoteHead 'pes-or-flexa #t)
                               'Bottom))))
                           (else
                            '()))
                          (begin
                            (set! previous-note item)
                            '())
                          (apply-note-repetitions
                           note
                           (apply-vaticana-note-features-2
                            note
                            (list
                             (apply-vaticana-note-features-1
                              note
                              (apply-episema-events
                               episema-events
                               (make-ly-note
                                (apply ly:make-pitch vaticana-pitch)
                                (ly:make-duration 2)
                                #f))))))
                          (cond
                           ((and is-melisma (eq? first-note item))
                            (list (context-spec-music (make-property-set 'melismaBusy #t) 'Bottom)))
                           ((and is-melisma (eq? last-note item))
                            (list (context-spec-music (make-property-unset 'melismaBusy) 'Bottom)))
                           (else '()))
                          (if (or (and is-melisma (eq? last-note item))
                                  is-single-note-special-head)
                              (list (make-music 'LigatureEvent 'span-direction 1))
                              '()))))
                      (('divisio type)
                       (filter
                        values
                        (list
                         (if (and (gabc:syl-has-lyrics? syllable)
                                  (not (syl-has-decorated-notes? syllable)))
                             ;; lyrics under a divisio are very common in gabc,
                             ;; but unsupported in LilyPond
                             (make-invisible-note)
                             #f)
                         (primitive-eval (or (assoc-ref vaticana-divisiones-mapping type)
                                             default-vaticana-bar)))))
                      (('space type)
                       (let*
                           ((pitch-nums (third previous-note))
                            ;; TODO extract the modern vs. square notation octave arithmetic
                            (previous-pitch (apply ly:make-pitch (cons (- (second pitch-nums) 1) (list-tail pitch-nums 2))))
                            (one-step-below (ly:pitch-transpose previous-pitch (ly:make-pitch -1 5 1/2)))) ; 1 diatonic step under the last note
                         (cond
                          ((string=? " " type)
                           (invisible-note-on-pitch previous-pitch))
                          ((string=? "//" type)
                           (invisible-note-on-pitch one-step-below))
                          ((string=? "/" type)
                           (list
                            (once hideNotes) (once teeny)
                            (make-ly-note one-step-below (ly:make-duration 2) #f)))
                          (else '()))))
                      (any #f))))
                items-with-episema-events)))))))
       syllables))
     'VaticanaVoice
     context-id)))

(define (apply-episema-events events ly-note)
  (cond
   ((= 0 (length events))
    ly-note)
   ((< 1 (length events))
    (apply-single-note-episema ly-note))
   ((eq? 'open (car events))
    (open-episema ly-note))
   (else
    (close-episema ly-note))))

;; apply features that are music functions
(define (apply-vaticana-note-features-1 gabc-note ly-note)
  (let ((tests-and-transformations
         `((,gabc:note-has-punctum-mora? . ,augmentum)
           (,(lambda (x) (< 1 (gabc:note-punctum-mora-count x))) . ,augmentum)
           (,gabc:note-has-ictus? . ,(cut apply-articulation ictus <>))
           (,gabc:note-has-accentus? . ,(cut apply-articulation accentus <>))
           (,gabc:note-has-accent-grave? . ,(cut apply-articulation lilygabcAccentGrave <>))
           (,gabc:note-has-circulus? . ,(cut apply-articulation circulus <>))
           (,gabc:note-has-semicirculus? . ,(cut apply-articulation semicirculus <>))
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
(define (apply-vaticana-note-features-2 gabc-note ly-note-list)
  (let ((tests-and-transformations
         `((,gabc:note-is-diminutive? . ,deminutum)
           (,gabc:note-is-punctum-inclinatum? . ,inclinatum)
           (,gabc:note-is-debilis? . ,deminutum)
           (,gabc:note-is-virga? . ,virga)
           (,(lambda (x)
               (and (gabc:note-is-ascendens? x)
                    (not (gabc:note-is-stropha? x))))
            . ,ascendens)
           (,gabc:note-is-ascendens? . ,auctum)
           (,gabc:note-is-descendens? . ,descendens)
           (,gabc:note-is-descendens? . ,auctum)
           (,gabc:note-is-quilisma? . ,quilisma)
           (,gabc:note-is-oriscus? . ,oriscus)
           (,gabc:note-is-cavum? . ,cavum)
           (,gabc:note-has-linea? . ,linea)
           (,gabc:note-is-stropha? . ,stropha))))
    (fold
     (lambda (x r)
       (match-let (((test . transformation) x))
         (if (test gabc-note)
             (append (list transformation) r)
             r)))
     ly-note-list
     tests-and-transformations)))

(define (invisible-note-on-pitch pitch)
  (list
   (once hideNotes)
   (make-ly-note pitch (ly:make-duration 2) #f)))

(define gabc-vaticana
  (define-scheme-function
    (input)
    (string?)
    (let*
        ((score (gabc:parse input))
         (score-with-pitches (pitch:decorate-notes score))
         (context-id "uniqueContext0")
         (has-lyrics (any gabc:syl-has-lyrics? (util:flatten score)))
         (notes (make-vaticana-notes score-with-pitches (if has-lyrics context-id #f))))
      (set! (ly:music-property notes 'create-new) #t)
      (if has-lyrics
          (make-simultaneous-music
           (list
            notes
            (make-lyrics score context-id 'VaticanaLyrics)))
          notes))))

(define gabc-vaticana-file
  (define-scheme-function
    (path)
    (string?)
    (gabc-vaticana
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
