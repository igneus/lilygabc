;; Operates on the output files.
;; Compares expected and actual output of each test example,
;; reports results.

(use-modules
 (ice-9 getopt-long)
 (ice-9 match)
 (ice-9 rdelim)
 (ice-9 textual-ports)
 (srfi srfi-1)
 (srfi srfi-26))

(define (load-examples path)
  (call-with-input-file
   path
   (lambda (file)
     (let ((result '()) (title #f) (content ""))
       (do ((line (read-line file) (read-line file))) ((eof-object? line))
         (if (string-prefix? "% test" line)
             (begin
               (when title
                 (set! result (append result (list (cons title content)))))
               (set! title line)
               (set! content ""))
             (set! content (string-append content line "\n"))))
       (when title
         (set! result (append result (list (cons title content)))))
       result))))

(define (show-diff a b)
  (let ((fa "tmp1.out") (fb "tmp2.out"))
    (call-with-output-file fa (cut put-string <> a))
    (call-with-output-file fb (cut put-string <> b))
    (system* "diff" "--color" "-U3" fa fb)))



(load "options.scm")

(let*
    ((example-pattern (option-ref options 'example #f))
     (all-examples (zip (load-examples "expected.out") (load-examples "actual.out")))
     (run-examples (if example-pattern
                       (filter (lambda (x) (string-contains (car (first x)) example-pattern)) all-examples)
                       all-examples))
     (failures
      (filter
       (lambda (x)
         (match-let*
             ((((expected-name . expected-text) (actual-name . actual-text)) x))
           (unless (string=? expected-name actual-name)
             (error "Examples zipped together don't belong to the same test"))
           (let ((is-success (string=? expected-text actual-text)))
             (if is-success
                 (display (string-append "pass " expected-name "\n"))
                 (begin
                   (display (string-append "FAIL " expected-name ":\n"))
                   (show-diff expected-text actual-text)))
             (not is-success))))
       run-examples)))

  (unless (= 0 (length failures))
    (display "\nFailed examples:\n")
    (for-each
     (lambda (x) (display (car (first x))) (newline))
     failures))
  (newline)

  (display (string-append
            (number->string (length failures))
            " failures, "
            (number->string (length run-examples))
            " examples total\n"))

  (when (= 0 (length failures))
    (display "ALL TESTS GREEN\n")))
