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
         (syllable-items (util:flatten (util:flatten words))))
      (make-music
       'ContextSpeccedMusic
       'element
       (make-music
        'SequentialMusic
        'elements
        (util:flatten
         (filter-map
          (lambda (item)
            (match item
              (('clef type line clef-is-flat)
               (let* ((lily-clefnum (- line 1))
                      (actual-clefnum ; LilyPond doesn't provide chant clefs for all 4 lines. Fallbacks:
                       (cond
                        ((eq? lily-clefnum 0) 1)
                        ((and (string=? type "f") (eq? lily-clefnum 3)) 2)
                        (else lily-clefnum))))
                 (append
                  (list (clef (string-append
                               "vaticana-"
                               (if (string=? "f" type) "fa" "do")
                               (number->string actual-clefnum))))
                  (if clef-is-flat (list key-flat) '()))))
              (('note-with-pitch note pitch)
               ;; shift the pitch an octave lower:
               ;; LilyPond treats the chant c clef as denoting middle c
               (let ((vaticana-pitch (cons (- (second pitch) 1) (list-tail pitch 2))))
                 (list
                  (make-ly-note
                   (apply ly:make-pitch vaticana-pitch)
                   (ly:make-duration 2)
                   #f))))
              (('divisio type)
               (list
                (primitive-eval (or (assoc-ref vaticana-divisiones-mapping type)
                                    default-vaticana-bar))))
              (any #f)))
          syllable-items)))
       'context-type 'VaticanaVoice
       'property-operations '()
       'create-new #t))))

(define gabc-vaticana-file
  (define-scheme-function
    (path)
    (string?)
    (gabc-vaticana
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
