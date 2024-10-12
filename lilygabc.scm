(use-modules
 (ice-9 regex)
 (ice-9 textual-ports)
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

(define (make-notes words context-id)
  (make-music
   'ContextSpeccedMusic
   'context-id
   context-id
   'context-type
   'Voice
   'element
   (make-music
    'SequentialMusic
    'elements
    (util:flatten
     (map
      (lambda (syllable)
        (let* ((notes (filter (lambda (x) (eq? 'note-with-pitch (first x))) syllable))
               (is-melisma (< 1 (length notes)))
               (is-melisma-beginning (lambda (i) (= 1 i)))
               (is-melisma-end (lambda (i) (= (length notes) i)))
               (note-i 0))
          (cond
           ((eq? '() syllable) ; void syllable rendered as invisible bar
            (list (make-music 'BarEvent 'bar-type "")))
           ((and (gabc:syl-has-lyrics? syllable)
                 (not (gabc:syl-has-notes? syllable))
                 (= 1 (length syllable))) ; the syllable has only lyrics, no renderable music elements
            (list (make-invisible-note)))
           (else
            (util:flatten
             (filter-map
              (lambda (item)
                (case (first item)
                  ((note-with-pitch)
                   (set! note-i (+ 1 note-i))
                   (let ((note (second item))
                         (pitch-nums (list-tail (third item) 1)))
                     (list
                      (apply-note-features
                       note
                       (make-ly-note
                        (apply ly:make-pitch pitch-nums)
                        (if (gabc:note-has-punctum-mora? note)
                            (ly:make-duration 2 1)
                            (ly:make-duration 2))
                        (if is-melisma
                            (cond ((is-melisma-beginning note-i) -1)
                                  ((is-melisma-end note-i) 1)
                                  (else #f))
                            #f))))))
                  ((divisio)
                   (let* ((lilybar
                           (or (assoc-ref divisiones-mapping (second item))
                               default-bar))
                          (bartype (cdr lilybar)))
                     (filter
                      values
                      (list
                       (if (and (gabc:syl-has-lyrics? syllable)
                                (not (gabc:syl-has-notes? syllable)))
                           ;; lyrics under a divisio are very common in gabc,
                           ;; but unsupported in LilyPond
                           (make-invisible-note)
                           #f)
                       (if (eq? 'breathe (car lilybar))
                           (breathe)
                           (make-music 'BarEvent 'bar-type bartype))))))
                  (else #f)))
              syllable))))))
      (util:flatten words))))))

(define (make-ly-note pitch duration slur-direction)
  (apply
   make-music
   (append
    (list 'NoteEvent)
    (if slur-direction
        (list 'articulations
              (list (make-music 'SlurEvent 'span-direction slur-direction)))
        '())
    (list
     'duration duration
     'pitch pitch))))

;; apply features of the gabc note
;; on a modern notation LilyPond note
(define (apply-note-features gabc-note ly-note)
  (let* ((virga-side (gabc:note-virga-side gabc-note))
         (tests-and-transformations
          `((,gabc:note-is-punctum-inclinatum? . ,tiny-note)
            (,gabc:note-is-diminutive? . ,teeny-note)
            (,gabc:note-has-ictus? . ,apply-ictus)
            (,gabc:note-has-horizontal-episema? . ,apply-horizontal-episema)
            (,gabc:note-virga-side . ,(cut apply-virga virga-side <>)))))
    (fold
     (lambda (x r)
       (let ((test (car x))
             (transformation (cdr x)))
         (if (test gabc-note)
             (transformation r)
             r)))
     ly-note
     tests-and-transformations)))

(define (make-lyrics words context-id)
  (make-music
   'ContextSpeccedMusic
   'element
   (make-music
    'LyricCombineMusic
    'associated-context-type
    'Voice
    'associated-context
    context-id
    'element
    (make-music
     'SequentialMusic
     'elements
     (util:flatten
      (map
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
                (if (and (> (length word) 1)
                         (not (eq? lyr (last lyrics))))
                    (list 'articulations
                          (list (make-music 'HyphenEvent)))
                    '())
                (list
                 'text
                 (second lyr)
                 'duration
                 (ly:make-duration 2)))))
            lyrics)))
       words))
     (make-music 'CompletizeExtenderEvent)))
   'property-operations
   '()
   'context-type
   'Lyrics
   'create-new
   #t))

; accepts string containing gabc notation,
; returns LilyPond music
(define gabc
  (define-scheme-function
    (input)
    (string?)
    (let*
        ((score (gabc:parse input))
         (context-id "uniqueContext0"))
      (make-music
       'SimultaneousMusic
       'elements
       (list
        (make-notes (pitch:decorate-notes score) context-id)
        (make-lyrics score context-id))))))

(define gabc-file
  (define-scheme-function
    (path)
    (string?)
    (gabc
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
