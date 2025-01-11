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

 ;; digraphs

 ;; accented characters
 )

(test-end suite-name)
