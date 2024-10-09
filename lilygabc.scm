(add-to-load-path (dirname (current-filename)))

(use-modules
 (ice-9 regex)
 (srfi srfi-26)
 ((lilygabc gabc) #:prefix gabc:))

(define (gabc-note-to-pitch clef note)
  (let*
      ((clef-type (assoc-ref clef 'type))
       (clef-line (assoc-ref clef 'line))
       (clef-shift
        (* 2 (- 4 clef-line (if (string=? "f" clef-type) 2 0))))
       (note-index (list-index (cut char=? note <>) gabc:note-names))
       (note-num (+ 5 note-index clef-shift))
       (note (modulo note-num 7))
       (octave (- (truncate-quotient note-num 7) 1)))
    (ly:make-pitch octave note)))

; mapping Gregorio divisiones -> LilyPond bars
; (single line full height bar is default, not included)
(define divisiones-mapping
  '(("," . "'")
    (";" . ",")
    ("::" . "||")))

; accepts string containing gabc notation,
; returns LilyPond music
(define music-from-gabc-string
  (define-scheme-function
    (input)
    (string?)
    (let*
        ((clef
          (gabc:find-clef input))
         (syllables
          (gabc:parse input))
         (flatten
          (cut apply append <>))
         (note-name
          (lambda (note) (string-ref (second note) 0)))
         (make-ly-note ; TODO extract to a separate function
          (lambda (note slur-direction)
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
               'pitch (gabc-note-to-pitch clef (note-name note))))))))
      (make-music
       'SimultaneousMusic
       'elements
       (list
        (make-music ; notes
         'ContextSpeccedMusic
         'context-id
         "uniqueContext0"
         'context-type
         'Voice
         'element
         (make-music
          'SequentialMusic
          'elements
          (flatten
           (map
            (lambda (syllable)
              (let* ((notes (filter (lambda (x) (eq? 'note (car x))) syllable))
                     (is-melisma (< 1 (length notes)))
                     (is-melisma-beginning (lambda (i) (= 1 i)))
                     (is-melisma-end (lambda (i) (= (length notes) i)))
                     (note-i 0))
                (filter-map
                 (lambda (item)
                   (cond
                    ((eq? 'note (first item))
                     (set! note-i (+ 1 note-i))
                     (make-ly-note
                      item
                      (if is-melisma
                          (cond ((is-melisma-beginning note-i) -1)
                                ((is-melisma-end note-i) 1)
                                (else #f))
                          #f)))
                    ((eq? 'divisio (first item))
                     (make-music 'BarEvent 'bar-type
                                 (or (assoc-ref divisiones-mapping (second item))
                                     "|")))
                    (else #f)))
                 syllable)))
            syllables))))
        (make-music ; lyrics
         'ContextSpeccedMusic
         'element
         (make-music
          'LyricCombineMusic
          'associated-context-type
          'Voice
          'associated-context
          "uniqueContext0"
          'element
          (make-music
           'SequentialMusic
           'elements
           (flatten
            (filter-map
             (lambda (syllable)
               (let ((lyr (first syllable)))
                 (if (eq? 'lyrics (first lyr))
                     (list
                      (make-music
                       'LyricEvent
                       'text
                       (second lyr)
                       'duration
                       (ly:make-duration 2))
                      (make-music 'CompletizeExtenderEvent))
                     #f)))
             syllables))))
         'property-operations
         '()
         'context-type
         'Lyrics
         'create-new
         #t))))))
