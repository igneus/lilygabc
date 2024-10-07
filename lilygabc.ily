% Load gabc scores and render them in LilyPond

#(use-modules (ice-9 regex))

#(define gabc-note-names
  '((#\c . 0)
    (#\d . 1)
    (#\e . 2)
    (#\f . 3)
    (#\g . 4)
    (#\h . 5)
    (#\i . 6)))

#(define (gabc-note-to-pitch note)
  (ly:make-pitch 0 (assoc-ref gabc-note-names note)))

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
