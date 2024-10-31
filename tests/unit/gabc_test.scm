(use-modules (srfi srfi-64))
(use-modules (lilygabc gabc))

(define suite-name "lilygabc_unit_tests")

(test-begin suite-name)

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

 ;; clefs
 (test-equal '((((clef "c" 4 #f))))
             (parse "(c4)"))
 (test-equal '((((clef "f" 3 #f))))
             (parse "(f3)"))
 (test-equal '((((clef "c" 4 #t))))
             (parse "(cb4)"))

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
 (test-equal '((((lyrics "la"))))
             (parse "<b>la</b>()"))

 ;; score header
 (test-equal '((((clef "c" 4 #f)))) ; header is ignored
             (parse "book: No Book;\n%%\n(c4)"))
 (test-equal '((((clef "c" 4 #f)))) ; DOS-style newlines
             (parse "book: No Book;\r\n%%\r\n(c4)"))
 ;; multiple headers: official Gregorio documentation doesn't specify
 ;; this behaviour, but the actual implementation allows multiple header
 ;; sections and GregoBase produces a lot of such gabc files
 (test-equal '((((clef "c" 4 #f)))) ; both headers ignored
             (parse "book: No Book;\n%%\nauthor: No Author;\n%%\n(c4)"))

 ;; comments
 (test-equal '((((note "g"))))
             (parse "(g) % (g) "))
 (test-equal '((((note "g"))) (((note "h"))))
             (parse "(g) % (g) \n (h) % (h)"))

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
 (test-equal '((((note "g" "-"))))
             (parse "(-g)"))
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
 (test-equal '((((note "g" "r0"))))
             (parse "(gr0)"))

 ;; articulations
 (test-equal '((((note "g" "."))))
             (parse "(g.)"))
 (test-equal '((((note "g" "'"))))
             (parse "(g')"))
 (test-equal '((((note "g" "_"))))
             (parse "(g_)"))
 (test-equal '((((note "g" "_'"))))
             (parse "(g_')"))

 ;; accidentals
 (test-equal '((((accidental "g" flat))))
             (parse "(gx)"))
 (test-equal '((((accidental "g" sharp))))
             (parse "(g#)"))
 (test-equal '((((accidental "g" natural))))
             (parse "(gy)"))

 ;; spaces
 (test-equal '((((space "!"))))
             (parse "(!)"))
 (test-equal '((((space "@"))))
             (parse "(@)"))
 (test-equal '((((space "/"))))
             (parse "(/)"))
 (test-equal '((((space "//"))))
             (parse "(//)"))
 (test-equal '((((space " "))))
             (parse "( )"))

 ;; line breaks
 (test-equal '((((line-break "z"))))
             (parse "(z)"))
 (test-equal '((((line-break "Z"))))
             (parse "(Z)"))
 (test-equal '((((line-break "z+"))))
             (parse "(z+)"))
 (test-equal '((((line-break "z-"))))
             (parse "(z-)"))

 ;; rendering settings and other features in square brackets
 ;; (in the music)
 (test-equal '((((note "e") (note "d") (square-brackets "ll:1"))))
             (parse "(ed[ll:1])")))

(test-group
 "parse-gly"

 (test-equal '((((clef "c" 4 #f)))
               (((note "g")))
               (((note "h") (space " ") (note "i"))))
             (parse-gly "c4 g (h i)")))

(test-group
 "note-repetitions"

 (test-equal #f (note-repetitions '(note "g")))
 (test-equal #f (note-repetitions '(note "g" "v")))
 (test-equal 2  (note-repetitions '(note "g" "vv")))
 (test-equal 3  (note-repetitions '(note "g" "vvv")))
 (test-equal 3  (note-repetitions '(note "g" "sss"))))

(test-group
 "note-is-cavum?"
 (test-equal #f (note-is-cavum? '(note "g")))
 (test-equal #t (note-is-cavum? '(note "g" "r")))
 (test-equal #t (note-is-cavum? '(note "g" "r_")))
 (test-equal #f (note-is-cavum? '(note "g" "r1"))))

(test-end suite-name)
