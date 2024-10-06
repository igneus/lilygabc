% Load gabc scores and render them in LilyPond

music-from-gabc-string =
#(define-scheme-function
  (input)
  (string?)
  (let
   ((gabc-notes '((#\c . 0) (#\d . 1) (#\e . 2) (#\f . 3) (#\g . 4) (#\h . 5) (#\i . 6))))
   (make-music
    'SequentialMusic
    'elements
    (list (make-music
           'NoteEvent
           'duration
           (ly:make-duration 2)
           'pitch
           (ly:make-pitch 0 (assoc-ref gabc-notes (string-ref input 6))))))))
