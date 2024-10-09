(use-modules
 (ice-9 regex)
 (ice-9 textual-ports)
 (srfi srfi-26)
 ((lilygabc gabc) #:prefix gabc:)
 ((lilygabc util) #:prefix util:))

(define (gabc-note-to-pitch clef note)
  (let*
      ((clef-type (assoc-ref clef 'type))
       (clef-line (assoc-ref clef 'line))
       (clef-shift
        (* 2 (- 4 clef-line (if (string=? "f" clef-type) 2 0))))
       (note-index (list-index (cut string=? note <>) gabc:note-names))
       (note-num (+ 5 note-index clef-shift))
       (note (modulo note-num 7))
       (octave (- (truncate-quotient note-num 7) 1)))
    (ly:make-pitch octave note)))

; mapping Gregorio divisiones -> LilyPond bars
(define divisiones-mapping
  '(("," . "'")
    (";" . ",")
    ("::" . "||")))
(define default-bar "|") ; used for all not explicitly mapped

(define (make-notes clef words context-id)
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
        (let* ((notes (filter (lambda (x) (eq? 'note (first x))) syllable))
               (is-melisma (< 1 (length notes)))
               (is-melisma-beginning (lambda (i) (= 1 i)))
               (is-melisma-end (lambda (i) (= (length notes) i)))
               (note-i 0))
          (cond
           ((eq? '() syllable) ; void syllable rendered as invisible bar
            (list (make-music 'BarEvent 'bar-type "")))
           ((and (gabc:syl-has-lyrics? syllable)
                 (not (gabc:syl-has-notes? syllable)))
            (list (make-invisible-note)))
           (else
            (filter-map
             (lambda (item)
               (case (first item)
                 ((note)
                  (set! note-i (+ 1 note-i))
                  (make-ly-note
                   (gabc-note-to-pitch clef (second item))
                   (if is-melisma
                       (cond ((is-melisma-beginning note-i) -1)
                             ((is-melisma-end note-i) 1)
                             (else #f))
                       #f)))
                 ((divisio)
                  (make-music 'BarEvent 'bar-type
                              (or (assoc-ref divisiones-mapping (second item))
                                  default-bar)))
                 (else #f)))
             syllable)))))
      (util:flatten words))))))

(define (make-ly-note pitch slur-direction)
  (apply
   make-music
   (append
    (list 'NoteEvent)
    (if slur-direction
        (list 'articulations
              (list (make-music 'SlurEvent 'span-direction slur-direction)))
        '())
    (list
     'duration (ly:make-duration 2)
     'pitch pitch))))

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
        ((clef
          (gabc:find-clef input))
         (words
          (gabc:parse input))
         (context-id "uniqueContext0"))
      (make-music
       'SimultaneousMusic
       'elements
       (list
        (make-notes clef words context-id)
        (make-lyrics words context-id))))))

(define gabc-file
  (define-scheme-function
    (path)
    (string?)
    (gabc
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
