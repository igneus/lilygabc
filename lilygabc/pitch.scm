;; translating gabc visual notation to LilyPond absolute pitch

(define-module (lilygabc pitch)
  #:export (note-pitch)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((lilygabc util) #:prefix util:))

;; list of gabc note names, in an ascending order, as strings
(define note-names (util:char-range #\a #\m))

;; Translates a gabc note
;; to a pitch specified by two integers specifying octave and step
;; (the system is the same as LilyPond uses).
(define (note-pitch clef note)
  (let*
      ((clef-type (assoc-ref clef 'type))
       (clef-line (assoc-ref clef 'line))
       (clef-shift
        (* 2 (- 4 clef-line (if (string=? "f" clef-type) 2 0))))
       (note-index (list-index (cut string=? (string-downcase (second note)) <>) note-names))
       (note-num (+ 5 note-index clef-shift))
       (step (modulo note-num 7))
       (octave (- (truncate-quotient note-num 7) 1)))
    (list octave step)))
