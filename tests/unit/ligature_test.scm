(use-modules
 (srfi srfi-64)
 (lilygabc ligature))

(define suite-name "ligature_unit_tests")

(test-begin suite-name)

(test-group
 "add-ligatures"
 (test-equal '()
             (add-ligatures '()))
 (test-equal '((note "g"))
             (add-ligatures '((note "g"))))
 (test-equal '((lyrics "La") (note "g"))
             (add-ligatures '((lyrics "La") (note "g"))))
 (test-equal '((note "g") (divisio ",") (note "g"))
             (add-ligatures '((note "g") (divisio ",") (note "g"))))

 ;; series of consecutive notes is a ligature
 (test-equal '((ligature open) (note "g") (note "g") (ligature close))
             (add-ligatures '((note "g") (note "g"))))

 ;; divisio breaks a ligature
 (test-equal '((ligature open) (note "g") (note "g") (ligature close)
               (divisio ",")
               (ligature open) (note "g") (note "g") (ligature close))
             (add-ligatures '((note "g") (note "g")
                              (divisio ",")
                              (note "g") (note "g"))))
 ;; neumatic space doesn't break a ligature
 (test-equal '((ligature open) (note "g") (space "/") (note "g") (ligature close))
             (add-ligatures '((note "g") (space "/") (note "g"))))
 ;; other elements don't break a ligature
 (test-equal '((ligature open) (note "g") (square-brackets "content") (note "g") (ligature close))
             (add-ligatures '((note "g") (square-brackets "content") (note "g"))))

 ;; special notehead, even on its own, must be in a ligature
 (test-equal '((ligature open) (note "G") (ligature close))
             (add-ligatures '((note "G"))))
 ;; special notehead before a divisio
 (test-equal '((ligature open) (note "G") (ligature close) (divisio ",") (note "g"))
             (add-ligatures '((note "G") (divisio ",") (note "g")))))

(test-end suite-name)
