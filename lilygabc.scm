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
          (parsed
           (gabc:parse input))
          (flatten
           (cut apply append <>))
          (notes
           (filter (lambda (x) (eq? 'note (first x)))
                   (flatten parsed)))
          (note-name
           (lambda (note) (string-ref (second note) 0))))
       (map
        (lambda (note)
          (make-music
           'NoteEvent
           'duration
           (ly:make-duration 2)
           'pitch
           (gabc-note-to-pitch clef (note-name note))))
        notes)))))
