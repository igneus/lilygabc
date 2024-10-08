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

; accepts string containing gabc notation,
; returns LilyPond music
(define music-from-gabc-string
  (define-scheme-function
    (input)
    (string?)
    (make-music
     'SequentialMusic
     'elements
     (let*
         ((clef
           (gabc:find-clef input))
          (syllables
           (gabc:parse input))
          (flatten
           (cut apply append <>))
          (note-name
           (lambda (note) (string-ref (second note) 0)))
          (make-ly-note
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
                'pitch (gabc-note-to-pitch clef (note-name note)))))))
          (slice
           (lambda (lst start end)
             (list-head (list-tail lst start) (- end start)))))
       (flatten
        (map
         (lambda (syllable)
           (let*
               ((notes (filter (lambda (x) (eq? 'note (car x))) syllable))
                (is-melisma (< 1 (length notes))))
             (if is-melisma
                 (append
                  (list (make-ly-note (first notes) -1))
                  (map (cut make-ly-note <> #f)
                       (slice notes 1 (- (length notes) 1)))
                  (list (make-ly-note (last notes) 1)))
                 (map (cut make-ly-note <> #f) notes))))
         syllables))))))
