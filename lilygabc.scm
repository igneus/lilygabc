(use-modules
 (ice-9 regex)
 (srfi srfi-26))

(define gabc-note-names
  (let*
      ((irange ; integer range, final element included
        (lambda (x y) (iota (+ (- y x) 1) x)))
       (char-range
        (lambda (x y)
          (map integer->char
               (apply irange (map char->integer (list x y)))))))
    (char-range #\a #\m)))

(define (gabc-note-to-pitch note)
  (let*
      ((note-index (list-index (cut char=? note <>) gabc-note-names))
       (note-num (+ 5 note-index))
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
         ((matched-neumes
           (list-matches "\\(([a-z])[^1-4]" input))
          (match-note-name
           (lambda (match) (string-ref (match:substring match 1) 0))))
       (map
        (lambda (match)
          (make-music
           'NoteEvent
           'duration
           (ly:make-duration 2)
           'pitch
           (gabc-note-to-pitch (match-note-name match))))
        matched-neumes)))))
