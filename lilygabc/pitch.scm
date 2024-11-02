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
(define* (note-pitch clef note #:key (base-octave 0))
  (let*
      ((clef-type (second clef))
       (clef-line (third clef))
       (clef-shift
        (* 2 (- 4 clef-line (if (string=? "f" clef-type) 2 0))))
       (note-index (list-index (cut string=? (string-downcase (second note)) <>) note-names))
       (note-num (+ 5 note-index clef-shift))
       (step (modulo note-num 7))
       (octave (+ (- (truncate-quotient note-num 7) 1) base-octave))
       (accidental
        (if (and (eq? 'note (first note))
                 (gabc:note-has-musica-ficta? note))
            (gabc:note-ficta-value note)
            0)))
    (if (= 0 accidental)
        (list octave step)
        (list octave step accidental))))

;; Octave step to which an accidental is related, as an integer
(define-public (accidental-step clef accidental)
  (second (note-pitch clef accidental))) ; abuse the similarity of note and accidental data structures

;; default clef assumed when the score hasn't (yet) specified any
(define default-clef '(clef "c" 3 #f))

;; Decorates each note with pitch
(define* (decorate-notes score #:key (base-octave 0))
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
               (let* ((pitch (note-pitch clef x #:base-octave base-octave))
                      (pitch-step (second pitch))
                      (accidental (assoc-ref active-accidentals pitch-step)))
                 (append
                  (list
                   'note-with-pitch
                   x
                   (append
                    '(pitch)
                    pitch
                    (cond
                     ((gabc:note-has-musica-ficta? x)
                      '()) ; accidental already populated by 'note-pitch'
                     (accidental
                      (list (if (eq? 'sharp accidental) 1/2 -1/2)))
                     (else '())))))))
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

(define-public (pitch=? a b)
  (and (= (second a) (second b))
       (= (third a) (third b))))
