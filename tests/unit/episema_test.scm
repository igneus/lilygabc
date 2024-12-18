(use-modules
 (srfi srfi-64)
 ((lilygabc gabc) #:prefix gabc:)
 (lilygabc episema))

(define suite-name "episema_unit_tests")

(test-begin suite-name)

(test-group
 "episema-events-default"

 ;; empty input
 (test-equal '()
             (episema-events-default '()))

 ;; episema-irrelevant input
 (test-equal '(())
             (episema-events-default '((note "g"))))
 (test-equal '(())
             (episema-events-default '((space "/"))))

 ;; single-note episema
 (test-equal '((open close))
             (episema-events-default '((note "g" "_"))))
 (test-equal '(() (open close) ())
             (episema-events-default '((note "g") (note "g" "_") (note "g"))))

 ;; multi-note episema
 (test-equal '((open) (close))
             (episema-events-default '((note "g" "_") (note "f" "_"))))
 (test-equal '((open) () (close))
             (episema-events-default '((note "g" "_") (note "h" "_") (note "f" "_"))))
 (test-equal '(() (open) (close) ())
             (episema-events-default '((note "g") (note "g" "_") (note "f" "_") (note "g"))))
 (test-equal '((open close) () (open close))
             (episema-events-default '((note "g" "_") (space "/") (note "f" "_"))))
 )

(test-group
 "decorate-notes"
 (test-equal '((note-with-episema-events (note "g" "_") (open close))
               (space "/"))
             (decorate-notes
              (lambda (x) (eq? 'note (car x)))
              gabc:note-has-horizontal-episema?
              '((note "g" "_") (space "/")))))

(test-end suite-name)
