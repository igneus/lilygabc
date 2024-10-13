(use-modules (srfi srfi-64))
(use-modules (lilygabc pitch))

(define suite-name "pitch_unit_tests")

(test-begin suite-name)

(let
    ((c4-clef '(clef "c" 4 #f))
     (c1-clef '(clef "c" 1 #f)))
  (test-group
   "note-pitch"
   (test-equal '(0 0)
               (note-pitch c4-clef '(note "c")))
   (test-equal '(0 6)
               (note-pitch c4-clef '(note "i")))
   (test-equal '(-1 5)
               (note-pitch c4-clef '(note "a")))
   (test-equal '(1 3)
               (note-pitch c4-clef '(note "m")))

   (test-equal '(1 0)
               (note-pitch c1-clef '(note "d"))))

  (test-group
   "accidental-step"
   (test-equal 6
               (accidental-step c4-clef '(accidental "i" 'flat)))))

(test-group
 "decorate-notes"

 ;; non-note elements are left alone
 (let ((input '((((lyrics "la"))))))
   (test-equal input (decorate-notes input)))

 ;; no clef, assume c4 as default
 (test-equal '((((note-with-pitch (note "j") (pitch 1 0)))))
             (decorate-notes '((((note "j"))))))
 (test-equal '((((note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((note "i"))))))

 ;; clef specified
 (test-equal
  '((((clef "c" 3 #f)
      (note-with-pitch (note "j") (pitch 1 2)))))
  (decorate-notes '((((clef "c" 3 #f) (note "j"))))))

 ;; accidentals:
 ;; - natural on its own has no effect
 (test-equal '((((accidental "i" natural)
                 (note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((accidental "i" natural)
                                 (note "i"))))))
 ;; - the same note is affected
 (test-equal '((((accidental "i" flat)
                 (note-with-pitch (note "i") (pitch 0 6 -1/2)))))
             (decorate-notes '((((accidental "i" flat)
                                 (note "i"))))))
 (test-equal '((((accidental "i" sharp)
                 (note-with-pitch (note "i") (pitch 0 6 1/2)))))
             (decorate-notes '((((accidental "i" sharp)
                                 (note "i"))))))
 ;; - different note is not affected
 (test-equal '((((accidental "g" flat)
                 (note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((accidental "g" flat)
                                 (note "i"))))))
 ;; - natural cancels the accidental
 (test-equal '((((accidental "i" flat)
                 (note-with-pitch (note "i") (pitch 0 6 -1/2))
                 (accidental "i" natural)
                 (note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((accidental "i" flat)
                                 (note "i")
                                 (accidental "i" natural)
                                 (note "i"))))))
 ;; - the same note in the next word is never more affected
 (test-equal '((((accidental "i" flat)
                 (note-with-pitch (note "i") (pitch 0 6 -1/2))))
               (((note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((accidental "i" flat)
                                 (note "i")))
                               (((note "i")))))) ; new word

 ;; clefs with accidentals:
 ;; - b is flat
 (test-equal '((((clef "c" 4 #t)
                 (note-with-pitch (note "i") (pitch 0 6 -1/2)))))
             (decorate-notes '((((clef "c" 4 #t)
                                 (note "i"))))))
 ;; - b in the next word is also flat
 (test-equal '((((clef "c" 4 #t)
                 (note-with-pitch (note "i") (pitch 0 6 -1/2))))
               (((note-with-pitch (note "i") (pitch 0 6 -1/2)))))
             (decorate-notes '((((clef "c" 4 #t)
                                 (note "i")))
                               (((note "i"))))))
 ;; - natural cancels the clef accidental
 (test-equal '((((clef "c" 4 #t)
                 (accidental "i" natural)
                 (note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((clef "c" 4 #t)
                                 (accidental "i" natural)
                                 (note "i"))))))
 ;; - ... but only until the end of the word
 (test-equal '((((clef "c" 4 #t)
                 (accidental "i" natural)))
               (((note-with-pitch (note "i") (pitch 0 6 -1/2)))))
             (decorate-notes '((((clef "c" 4 #t)
                                 (accidental "i" natural)))
                               (((note "i"))))))
 ;; - new clef without accidental cancels the previous clef accidental
 (test-equal '((((clef "c" 4 #t)
                 (clef "c" 4 #f)))
               (((note-with-pitch (note "i") (pitch 0 6)))))
             (decorate-notes '((((clef "c" 4 #t)
                                 (clef "c" 4 #f)))
                               (((note "i"))))))
 )

(test-end suite-name)
