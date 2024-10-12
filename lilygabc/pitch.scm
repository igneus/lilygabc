;; translating gabc visual notation to LilyPond absolute pitch

(define-module (lilygabc pitch)
  #:export (note-pitch
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

;; default clef assumed when the score hasn't (yet) specified any
(define default-clef '(clef "c" 4 #f))

;; Decorates each note with pitch
(define (decorate-notes score)
  (let ((clef default-clef))
    (map
     (lambda (word)
       (let ((active-accidentals '()))
         (util:map2
          (lambda (x)
            (case (first x)
              ((clef)
               (set! clef x)
               x)
              ((note)
               (let ((accidental (assoc-ref active-accidentals (string-downcase (second x)))))
                 (append
                  (list
                   'note-with-pitch
                   x
                   (append
                    '(pitch)
                    (note-pitch clef x)
                    (if accidental
                        (list (if (eq? 'sharp accidental) 1/2 -1/2))
                        '()))))))
              ((accidental)
               (set! active-accidentals
                     (if (eq? 'natural (third x))
                         (assoc-remove! active-accidentals (second x))
                         (assoc-set! active-accidentals (second x) (third x))))
               x)
              (else x)))
          word)))
     score)))
