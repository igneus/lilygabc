(use-modules (srfi srfi-64))
(use-modules (lilygabc pitch))

(define suite-name "pitch_unit_tests")

(test-begin suite-name)

(test-group
 "note-pitch"
 (let
     ((c4-clef '((type . "c") (line . 4)))
      (c1-clef '((type . "c") (line . 1))))

   (test-equal '(0 0)
               (note-pitch c4-clef '(note "c")))
   (test-equal '(-1 5)
               (note-pitch c4-clef '(note "a")))
   (test-equal '(1 3)
               (note-pitch c4-clef '(note "m")))

   (test-equal '(1 0)
               (note-pitch c1-clef '(note "d")))))

(test-end suite-name)
