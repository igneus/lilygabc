(use-modules (srfi srfi-64))
(use-modules (lilygabc util))

(define suite-name "util_unit_tests")

(test-begin suite-name)

(test-group
 "flatten"
 (test-equal '()
             (flatten '()))
 (test-equal '()
             (flatten '(())))
 (test-equal '(1 2 3)
             (flatten '((1 2) (3))))
 (test-equal '(1 2)
             (flatten '((1) () (2)))))

(test-group
 "split-at"
 (test-equal '()
             (split-at (lambda (x) #t) '()))
 (test-equal '((1))
             (split-at (lambda (x) #t) '(1)))
 (test-equal '((1) (2))
             (split-at (lambda (x) #t) '(1 2)))
 (test-equal '((1 2 3) (1 4))
             (split-at (lambda (x) (= 1 x)) '(1 2 3 1 4)))
 (test-equal '(((1 2)) ((3) (1 4)))
             (split-at (lambda (x) (= 1 (length x))) '((1 2) (3) (1 4)))))

(test-group
 "map2"
 (test-equal '()
             (map2 (lambda (x) x) '()))
 (test-equal '(())
             (map2 (lambda (x) x) '(())))
 (test-equal '((2 4) (6 8))
             (map2 (lambda (x) (* 2 x)) '((1 2) (3 4)))))

(test-end suite-name)
