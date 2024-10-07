% Load gabc scores and render them in LilyPond

#(use-modules (ice-9 regex))
#(use-modules (srfi srfi-26))

#(define gabc-note-names
  (let*
   ((irange ; integer range, final element included
     (lambda (x y) (iota (+ (- y x) 1) x)))
    (char-range
     (lambda (x y)
      (map integer->char
       (apply irange (map char->integer (list x y)))))))
    (char-range #\a #\m)))

#(display gabc-note-names)

#(define (gabc-note-to-pitch note)
  (let*
   ((note-index (list-index (cut char=? note <>) gabc-note-names))
    (note-num (+ 5 note-index))
    (note (modulo note-num 7))
    (octave (- (truncate-quotient note-num 7) 1)))
   (ly:make-pitch octave note)))

music-from-gabc-string =
#(define-scheme-function
  (input)
  (string?)
  (make-music
   'SequentialMusic
   'elements
   (let*
    ((not-clef
      (lambda (match) (not (string=? "4" (match:substring match 2)))))
     (matched-neumes
      (filter not-clef (list-matches "\\(([a-z])(.)" input)))
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
     matched-neumes))))
