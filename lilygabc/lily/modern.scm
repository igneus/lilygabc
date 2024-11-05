;; Implementation of generating modern notation.
;; Requires the LilyPond runtime.

(define-module (lilygabc lily modern)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (lily) ; LilyPond Scheme API
  #:use-module ((lilygabc episema) #:prefix episema:)
  #:use-module ((lilygabc gabc) #:prefix gabc:)
  #:use-module ((lilygabc pitch) #:prefix pitch:)
  #:use-module ((lilygabc util) #:prefix util:)
  #:use-module ((lilygabc lily lilypond-globals) #:prefix l:)
  #:use-module (lilygabc lily music-functions))

; mapping Gregorio divisiones -> LilyPond bars
(define divisiones-mapping
  `(("," . ,(l:bar "'"))
    (";" . ,(l:bar ","))
    ("::" . ,(l:bar "||"))
    ("`" . ,(l:breathe))))
(define default-bar (l:bar "|")) ; used for all not explicitly mapped

;; equivalent of gabc:syl-has-notes? operating on the results
;; of pitch:decorate-notes
(define-public (syl-has-decorated-notes? syllable)
  (find pitch:is-note-with-pitch? syllable))

(define-public (make-notes words)
  (make-sequential-music
   (util:flatten
    (let ((last-clef-was-flat #f))
      (map
       (lambda (syllable)
         (let* ((notes (filter pitch:is-note-with-pitch? syllable))
                (is-melisma (< 1 (length notes)))
                (first-note (and is-melisma (first notes)))
                (last-note (and is-melisma (last notes)))
                (items-with-episema-events
                 (episema:decorate-notes
                  pitch:is-note-with-pitch?
                  (lambda (x) (gabc:note-has-horizontal-episema? (second x)))
                  syllable)))
           (cond
            ((eq? '() syllable) ; void syllable rendered as invisible bar
             (list (l:bar "")))
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
                   (('note-with-episema-events ('note-with-pitch note pitch) episema-events)
                    (apply-note-repetitions
                     note
                     (list
                      (apply-note-features
                       note
                       (apply-episema-events
                        episema-events
                        (make-ly-note
                         (apply ly:make-pitch (list-tail pitch 1))
                         (if (gabc:note-has-punctum-mora? note)
                             (ly:make-duration 2 (gabc:note-punctum-mora-count note))
                             (ly:make-duration 2))
                         (if is-melisma
                             (cond ((eq? (second item) first-note) -1)
                                   ((eq? (second item) last-note) 1)
                                   (else #f))
                             #f)))))))
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
                      (or (assoc-ref divisiones-mapping type)
                          default-bar))))
                   (('line-break type)
                    (list l:break))
                   (any #f)))
               items-with-episema-events))))))
       (util:flatten words))))))

(define-public (make-ly-note pitch duration slur-direction)
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

(define-public (apply-episema-events events ly-note)
  (cond
   ((= 0 (length events))
    ly-note)
   ((< 1 (length events))
    (apply-single-note-episema ly-note))
   ((eq? 'open (car events))
    (open-episema ly-note))
   (else
    (close-episema ly-note))))

;; apply features of the gabc note
;; on a modern notation LilyPond note
(define (apply-note-features gabc-note ly-note)
  (let* ((virga-side (gabc:note-virga-side gabc-note))
         (tests-and-transformations
          `((,gabc:note-is-punctum-inclinatum? . ,tiny-note)
            (,gabc:note-is-diminutive? . ,teeny-note)
            (,gabc:note-is-debilis? . ,teeny-note)
            (,gabc:note-has-ictus? . ,(cut apply-articulation-down l:staccatissimo <>))
            (,gabc:note-is-virga? . ,(cut apply-virga virga-side <>))
            (,gabc:note-is-quilisma? . ,(cut apply-articulation l:prall <>))
            (,gabc:note-has-musica-ficta? . ,apply-musica-ficta))))
    (fold
     (lambda (x r)
       (match-let (((test . transformation) x))
         (if (test gabc-note)
             (transformation r)
             r)))
     ly-note
     tests-and-transformations)))

(define-public (apply-note-repetitions gabc-note music)
  (let ((num (or (gabc:note-repetitions gabc-note) 1)))
    (append-map (lambda (i) music) (iota num))))

(define-public (make-lyrics words context-id lyrics-type)
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
