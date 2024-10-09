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

(test-group
 "parse"
 (test-equal '()
             (parse ""))
 (test-equal '()
             (parse "   "))

 (test-equal '((((note "c"))))
             (parse "(c)"))
 (test-equal '((((note "c"))) (((note "d"))))
             (parse "(c) (d)"))
 (test-equal '((((note "c") (note "d"))))
             (parse "(cd)"))

 (test-equal '((((clef "c" 4 #f))))
             (parse "(c4)"))

 (test-equal '((())) ; note: void syllable is rendered as \bar "" , allowing line-break when it's not allowed anywhere
             (parse "()"))

 (test-equal '((((divisio ","))))
             (parse "(,)"))
 (test-equal '((((divisio ";"))))
             (parse "(;)"))
 (test-equal '((((divisio ":"))))
             (parse "(:)"))
 (test-equal '((((divisio "::"))))
             (parse "(::)"))

 (test-equal '((((lyrics "la") (note "g"))))
             (parse "la(g)"))
 (test-equal '((((lyrics "la") (note "g")) ((lyrics "la") (note "g"))))
             (parse "la(g)la(g)"))
 (test-equal '((((lyrics "la"))))
             (parse "la()"))

 (test-equal '((((clef "c" 4 #f)))) ; header is ignored
             (parse "book: No Book;\n%%\n(c4)")))

(test-end suite-name)
