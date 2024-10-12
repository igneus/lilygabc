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

 ;; empty score
 (test-equal '()
             (parse ""))
 (test-equal '()
             (parse "   "))

 ;; music syllables and words
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

 ;; divisiones
 (test-equal '((((divisio ","))))
             (parse "(,)"))
 (test-equal '((((divisio ";"))))
             (parse "(;)"))
 (test-equal '((((divisio ":"))))
             (parse "(:)"))
 (test-equal '((((divisio "::"))))
             (parse "(::)"))
 (test-equal '((((divisio "`"))))
             (parse "(`)"))

 ;; lyrics
 (test-equal '((((lyrics "la") (note "g"))))
             (parse "la(g)"))
 (test-equal '((((lyrics "la") (note "g")) ((lyrics "la") (note "g"))))
             (parse "la(g)la(g)"))
 (test-equal '((((lyrics "la") (note "g"))) (((lyrics "la") (note "g"))))
             (parse "la(g) la(g)"))
 (test-equal '((((lyrics "la") (note "g"))) (((lyrics "la") (note "g"))))
             (parse "la(g)\nla(g)"))
 (test-equal '((((lyrics "la"))))
             (parse "la()"))

 ;; score header
 (test-equal '((((clef "c" 4 #f)))) ; header is ignored
             (parse "book: No Book;\n%%\n(c4)"))

 ;; nabc - adiastematic neumes
 (test-equal '((((lyrics "la") (note "g") (nabc "pu"))))
             (parse "la(g|pu)"))
 (test-equal '((((lyrics "la") (note "g") (nabc "vihg"))))
             (parse "la(g|vihg)"))

 ;; note shapes
 ;; http://gregorio-project.github.io/gabc/index.html#onenote
 (test-equal '((((note "g"))))
             (parse "(g)"))
 (test-equal '((((note "G"))))
             (parse "(G)"))
 (test-equal '((((note "g" "~"))))
             (parse "(g~)"))
 (test-equal '((((note "g" "<"))))
             (parse "(g<)"))
 (test-equal '((((note "g" ">"))))
             (parse "(g>)"))
 (test-equal '((((note "g" "o"))))
             (parse "(go)"))
 (test-equal '((((note "g" "o~"))))
             (parse "(go~)"))
 (test-equal '((((note "g" "o<"))))
             (parse "(go<)"))
 (test-equal '((((note "g" "w"))))
             (parse "(gw)"))
 (test-equal '((((note "g" "s"))))
             (parse "(gs)"))
 (test-equal '((((note "g" "s<"))))
             (parse "(gs<)"))
 (test-equal '((((note "g" "v"))))
             (parse "(gv)"))
 (test-equal '((((note "g" "V"))))
             (parse "(gV)"))

 ;; articulations
 (test-equal '((((note "g" "."))))
             (parse "(g.)"))
 (test-equal '((((note "g" "'"))))
             (parse "(g')"))
 (test-equal '((((note "g" "_"))))
             (parse "(g_)"))
 (test-equal '((((note "g" "_'"))))
             (parse "(g_')")))

(test-end suite-name)
