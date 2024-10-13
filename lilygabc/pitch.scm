;; translating gabc visual notation to LilyPond absolute pitch

(define-module (lilygabc pitch)
  #:export (note-pitch
            accidental-step
            decorate-notes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((lilygabc gabc) #:prefix gabc:)
  #:use-module ((lilygabc util) #:prefix util:))

;; list of gabc note names, in an ascending order, as strings
(define note-names (util:char-range #\a #\m))

;; Translates a gabc note
;; to a pitch specified by two integers specifying octave and step
;; (the system is the same as LilyPond uses).
(define (note-pitch clef note)
  (let*
      ((clef-type (second clef))
       (clef-line (third clef))
       (clef-shift
        (* 2 (- 4 clef-line (if (string=? "f" clef-type) 2 0))))
       (note-index (list-index (cut string=? (string-downcase (second note)) <>) note-names))
       (note-num (+ 5 note-index clef-shift))
       (step (modulo note-num 7))
       (octave (- (truncate-quotient note-num 7) 1)))
    (list octave step)))

;; Octave step to which an accidental is related, as an integer
(define (accidental-step clef accidental)
  (second (note-pitch clef accidental))) ; abuse the similarity of note and accidental data structures

;; default clef assumed when the score hasn't (yet) specified any
(define default-clef '(clef "c" 4 #f))

;; Decorates each note with pitch
(define (decorate-notes score)
  (let ((clef default-clef))
    (map
     (lambda (word)
       (let ((active-accidentals
              (if (gabc:clef-has-bflat? clef) '((6 . 'flat)) '())))
         (util:map2
          (lambda (x)
            (case (first x)
              ((clef)
               (set! clef x)
               (when (gabc:clef-has-bflat? clef)
                 (set! active-accidentals
                       (assoc-set! active-accidentals 6 'flat)))
               x)
              ((note)
               (let* ((pitch (note-pitch clef x))
                      (pitch-step (second pitch))
                      (accidental (assoc-ref active-accidentals pitch-step)))
                 (append
                  (list
                   'note-with-pitch
                   x
                   (append
                    '(pitch)
                    pitch
                    (if accidental
                        (list (if (eq? 'sharp accidental) 1/2 -1/2))
                        '()))))))
              ((accidental)
               (let ((step (accidental-step clef x))
                     (accidental-type (third x)))
                 (set! active-accidentals
                       (if (eq? 'natural accidental-type)
                           (assoc-remove! active-accidentals step)
                           (assoc-set! active-accidentals step accidental-type))))
               x)
              (else x)))
          word)))
     score)))
