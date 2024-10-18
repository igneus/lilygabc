(use-modules
 (ice-9 match)
 (ice-9 regex)
 (ice-9 textual-ports)
 (srfi srfi-26)
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

(define gabc-vaticana
  (define-scheme-function
    (input)
    (string?)
    (let*
        ((words (pitch:decorate-notes (gabc:parse input)))
         (syllables (util:flatten words)))
      (make-music
       'ContextSpeccedMusic
       'element
       (make-music
        'SequentialMusic
        'elements
        (util:flatten
         (map
          (lambda (syllable)
            (let* ((notes (filter (lambda (x) (eq? 'note-with-pitch (first x))) syllable))
                   (is-melisma (< 1 (length notes)))
                   (first-note (and is-melisma (first notes)))
                   (last-note (and is-melisma (last notes)))
                   (previous-note #f))
              (util:flatten
               (filter-map
                (lambda (item)
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
                         ((and is-melisma (eq? first-note item))
                          (list (make-music 'LigatureEvent 'span-direction -1)))
                         ((and is-melisma (not (pitch:pitch=? pitch (third previous-note))))
                          (list
                           (context-spec-music
                            (make-music
                             'OverrideProperty
                             'symbol 'NoteHead
                             'grob-property-path '(pes-or-flexa)
                             'grob-value #t
                             'pop-first #t
                             'once #t)
                            'Bottom)))
                         (else
                          '()))
                        (begin
                          (set! previous-note item)
                          '())
                        (list
                         (make-ly-note
                          (apply ly:make-pitch vaticana-pitch)
                          (ly:make-duration 2)
                          #f))
                        (cond
                         ((and is-melisma (eq? first-note item))
                          (list (context-spec-music (make-property-set 'melismaBusy #t) 'Bottom)))
                         ((and is-melisma (eq? last-note item))
                          (list (context-spec-music (make-property-unset 'melismaBusy) 'Bottom)))
                         (else '()))
                        (if (and is-melisma (eq? last-note item))
                            (list (make-music 'LigatureEvent 'span-direction 1))
                            '()))))
                    (('divisio type)
                     (list
                      (primitive-eval (or (assoc-ref vaticana-divisiones-mapping type)
                                          default-vaticana-bar))))
                    (any #f)))
                syllable))))
          syllables)))
       'context-type 'VaticanaVoice
       'property-operations '()
       'create-new #t))))

(define gabc-vaticana-file
  (define-scheme-function
    (path)
    (string?)
    (gabc-vaticana
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
