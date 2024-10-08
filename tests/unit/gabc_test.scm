(use-modules (srfi srfi-64))
(use-modules (lilygabc gabc))

(define suite-name "lilygabc_unit_tests")

(test-begin suite-name)

(test-group
 "find-clef"
 (test-equal '((type . "c") (line . 4) (b . #f))
             (find-clef "(c4)"))
 (test-equal '((type . "c") (line . 3) (b . #f))
             (find-clef "(c3)"))
 (test-equal '((type . "c") (line . 2) (b . #f))
             (find-clef "(c2)"))
 (test-equal '((type . "c") (line . 1) (b . #f))
             (find-clef "(c1)"))

 (test-equal '((type . "f") (line . 3) (b . #f))
             (find-clef "(f3)"))
 (test-equal '((type . "f") (line . 2) (b . #f))
             (find-clef "(f2)")))

(test-end suite-name)
