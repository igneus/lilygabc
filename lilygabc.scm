(use-modules
 (ice-9 match)
 (ice-9 regex)
 (ice-9 textual-ports)
 (srfi srfi-1)
 (srfi srfi-26)
 ((lilygabc gabc) #:prefix gabc:)
 ((lilygabc pitch) #:prefix pitch:)
 ((lilygabc util) #:prefix util:))

; mapping Gregorio divisiones -> LilyPond bars
(define divisiones-mapping
  '(("," . (bar . "'"))
    (";" . (bar . ","))
    ("::" . (bar . "||"))
    ("`" . (breathe . #f))))
(define default-bar '(bar . "|")) ; used for all not explicitly mapped

;; equivalent of gabc:syl-has-notes? operating on the results
;; of pitch:decorate-notes
(define (syl-has-decorated-notes? syllable)
  (find (lambda (x) (eq? 'note-with-pitch (first x))) syllable))

(define (make-notes words)
  (make-sequential-music
   (util:flatten
    (let ((last-clef-was-flat #f))
      (map
       (lambda (syllable)
         (let* ((notes (filter (lambda (x) (eq? 'note-with-pitch (first x))) syllable))
                (is-melisma (< 1 (length notes)))
                (first-note (and is-melisma (first notes)))
                (last-note (and is-melisma (last notes))))
           (cond
            ((eq? '() syllable) ; void syllable rendered as invisible bar
             (list (bar "")))
            ((and (gabc:syl-has-lyrics? syllable)
                  (= 1 (length syllable))) ; the syllable has only lyrics, no renderable music elements
             (list (make-invisible-note)))
            (else
             (util:flatten
              (filter-map
               (lambda (item)
                 (match item
                   (('clef type line clef-is-flat)
                    (if (eq? last-clef-was-flat clef-is-flat)
                        #f
                        (begin
                          (set! last-clef-was-flat clef-is-flat)
                          (list (if clef-is-flat key-flat key-natural)))))
                   (('note-with-pitch note pitch)
                    (apply-note-repetitions
                     note
                     (list
                      (apply-note-features
                       note
                       (make-ly-note
                        (apply ly:make-pitch (list-tail pitch 1))
                        (if (gabc:note-has-punctum-mora? note)
                            (ly:make-duration 2 1)
                            (ly:make-duration 2))
                        (if is-melisma
                            (cond ((eq? item first-note) -1)
                                  ((eq? item last-note) 1)
                                  (else #f))
                            #f))))))
                   (('divisio type)
                    (let* ((lilybar
                            (or (assoc-ref divisiones-mapping type)
                                default-bar))
                           (bartype (cdr lilybar)))
                      (filter
                       values
                       (list
                        (if (and (gabc:syl-has-lyrics? syllable)
                                 (not (syl-has-decorated-notes? syllable)))
                            ;; lyrics under a divisio are very common in gabc,
                            ;; but unsupported in LilyPond
                            (make-invisible-note)
                            #f)
                        (if (eq? 'breathe (car lilybar))
                            (breathe)
                            (bar bartype))))))
                   (any #f)))
               syllable))))))
       (util:flatten words))))))

(define (make-ly-note pitch duration slur-direction)
  (apply
   make-music
   (append
    (list 'NoteEvent)
    (list
     'pitch pitch
     'duration duration)
    (if slur-direction
        (list 'articulations
              (list (make-music 'SlurEvent 'span-direction slur-direction)))
        '()))))

;; apply features of the gabc note
;; on a modern notation LilyPond note
(define (apply-note-features gabc-note ly-note)
  (let* ((virga-side (gabc:note-virga-side gabc-note))
         (tests-and-transformations
          `((,gabc:note-is-punctum-inclinatum? . ,tiny-note)
            (,gabc:note-is-diminutive? . ,teeny-note)
            (,gabc:note-is-debilis? . ,teeny-note)
            (,gabc:note-has-ictus? . ,(cut apply-articulation-down staccatissimo <>))
            ;; TODO probably drop in favor of the actual episema
            (,gabc:note-has-horizontal-episema? . ,(cut apply-articulation-up tenuto <>))
            (,gabc:note-virga-side . ,(cut apply-virga virga-side <>))
            (,gabc:note-is-quilisma? . ,(cut apply-articulation prall <>)))))
    (fold
     (lambda (x r)
       (match-let (((test . transformation) x))
         (if (test gabc-note)
             (transformation r)
             r)))
     ly-note
     tests-and-transformations)))

(define (apply-note-repetitions gabc-note music)
  (let ((num (or (gabc:note-repetitions gabc-note) 1)))
    (append-map (lambda (i) music) (iota num))))

(define (make-lyrics words context-id lyrics-type)
  (make-music
   'ContextSpeccedMusic
   'create-new #t
   'context-type lyrics-type
   'property-operations '()
   'element
   (make-music
    'LyricCombineMusic
    'element
    (make-sequential-music
     (append
      (append-map
       (lambda (word)
         (let ((lyrics
                (filter-map
                 (lambda (syllable)
                   (cond
                    ((gabc:syl-has-lyrics? syllable)
                     (first syllable))
                    ((gabc:syl-has-notes? syllable)
                     '(lyrics ""))
                    (else #f)))
                 word)))
           (map
            (lambda (lyr)
              (apply
               make-music
               (append
                (list
                 'LyricEvent)
                (list
                 'duration (ly:make-duration 2)
                 'text (second lyr))
                (if (and (> (length word) 1)
                         (not (eq? lyr (last lyrics))))
                    (list 'articulations
                          (list (make-music 'HyphenEvent)))
                    '()))))
            lyrics)))
       words)
      (list (make-music 'CompletizeExtenderEvent))))
    'associated-context context-id
    'associated-context-type 'Voice)))

; accepts string containing gabc notation,
; returns LilyPond music
(define gabc
  (define-scheme-function
    (input)
    (string?)
    (let*
        ((score (gabc:parse input))
         (score-with-pitches (pitch:decorate-notes score))
         (context-id "uniqueContext0")
         (has-lyrics (any gabc:syl-has-lyrics? (util:flatten score)))
         (notes (make-notes score-with-pitches)))
      (if has-lyrics
          (make-simultaneous-music
           (list
            (context-spec-music notes 'Voice context-id)
            (make-lyrics score context-id 'Lyrics)))
          notes))))

(define gabc-file
  (define-scheme-function
    (path)
    (string?)
    (gabc
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
