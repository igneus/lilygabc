(use-modules (srfi srfi-64))
(use-modules (lilygabc lyrics))

(define suite-name "lyrics_unit_tests")

(test-begin suite-name)

(test-group
 "expand-special-chars"
 (test-equal ""
             (expand-special-chars ""))
 (test-equal "no special chars"
             (expand-special-chars "no special chars"))
 (test-equal "<sp>unsupported</sp>" ; <sp> tag with unsupported content is left as is
             (expand-special-chars "<sp>unsupported</sp>"))

 ;; barred letters
 (test-equal "℟"
             (expand-special-chars "<sp>R/</sp>"))
 (test-equal "℣"
             (expand-special-chars "<sp>V/</sp>"))

 (test-equal "pre ℟ post"
             (expand-special-chars "pre <sp>R/</sp> post"))
 (test-equal "℟℟"
             (expand-special-chars "<sp>R/</sp><sp>R/</sp>"))

 (test-equal "†"
             (expand-special-chars "<sp>+</sp>"))

 ;; digraphs
 (test-equal "æ"
             (expand-special-chars "<sp>ae</sp>"))
 (test-equal "œ"
             (expand-special-chars "<sp>oe</sp>"))
 (test-equal "Æ"
             (expand-special-chars "<sp>AE</sp>"))
 (test-equal "Œ"
             (expand-special-chars "<sp>OE</sp>"))

 ;; accented digraphs(test-equal "æ"
 (test-equal "ǽ"
             (expand-special-chars "<sp>'ae</sp>"))
 (test-equal "ǽ"
             (expand-special-chars "<sp>'æ</sp>"))
 (test-equal "œ́"
             (expand-special-chars "<sp>'oe</sp>"))
 )

(test-group
 "process-formatting"
 (test-equal '("no formatting")
             (process-formatting "no formatting"))
 (test-equal '((bold "text"))
             (process-formatting "<b>text</b>"))
 (test-equal '((bold "te") "xt")
             (process-formatting "<b>te</b>xt"))
 (test-equal '("te" (bold "xt"))
             (process-formatting "te<b>xt</b>"))
 (test-equal '((bold italic "text"))
             (process-formatting "<b><i>text</i></b>"))
 (test-equal '((bold italic "te") (bold "xt"))
             (process-formatting "<b><i>te</i>xt</b>"))
 (test-equal '((verbatim "\\ae"))
             (process-formatting "<v>\\ae</v>"))

 ;; how non-standard tag structures are handled
 ;; (Gregorio segfaults on most of these)
 (test-equal '("text")
             (process-formatting "<b>text"))
 (test-equal '("text")
             (process-formatting "text</b>"))
 (test-equal '((bold "t") (bold italic "ex") (italic "t"))
             (process-formatting "<b>t<i>ex</b>t</i>"))

 ;; unsupported tag
 (test-equal '("te" "xt") ; breaks the string in two, but does not add any formatting
             (process-formatting "te<s>xt</s>"))
 )

(test-group
 "remove-braces"
 (test-equal "La"
             (remove-braces "L{a}"))
 (test-equal "<v>L{a}</v>" ; braces inside the <v> tag are left intact
             (remove-braces "<v>L{a}</v>"))
 (test-equal "La<v>L{a}</v>La"
             (remove-braces "L{a}<v>L{a}</v>L{a}"))
 )

(test-end suite-name)
