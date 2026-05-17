;; Operates on the output files.
;; Compares expected and actual output of each test example,
;; reports results.

(use-modules
 (ice-9 getopt-long)
 (ice-9 match)
 (ice-9 rdelim)
 (ice-9 string-fun)
 (ice-9 textual-ports)
 (srfi srfi-1)
 (srfi srfi-26))

;; try to load module, define fallback if not available
(with-exception-handler
 (lambda (exn)
   (display "WARN: coloring module not found, using fallback\n\n")
   #f)
 (lambda ()
   ;; module from the Guile Library https://www.nongnu.org/guile-lib/doc/ ,
   ;; not available by default in LilyPond installations
   (use-modules ((term ansi-color) #:prefix ansi-color:)))
 #:unwind? #t)
(define colorize-string
  (let ((sym 'ansi-color:colorize-string))
    (if (defined? sym)
        (eval sym (current-module))
        (lambda (str colour)
          str))))

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

(define (load-example-pairs expected-path actual-path)
  (zip (load-examples expected-path)
       (load-examples actual-path)))

(define (show-diff unified a b)
  (let ((fa "tmp1.out") (fb "tmp2.out"))
    (call-with-output-file fa (cut put-string <> a))
    (call-with-output-file fb (cut put-string <> b))
    (system* "diff" "--color" "-U" unified fa fb)))



(load "options.scm")

(let*
    ((diff-unified (option-ref options 'unified "3"))
     (merge-batches (option-ref options 'merge-batches #f))
     (cmdline-paths
      (if merge-batches
          '("all.ly")
          (option-ref options '() '())))
     (added-paths (option-ref options 'report-add #f))
     (file-pairs
      ;; files specified as arguments
      (append
       (map
        (lambda (f)
          (let ((bn (basename f)))
            (list
             (string-replace-substring bn ".ly" "_expected.out")
             (string-replace-substring bn ".ly" "_actual.out"))))
        cmdline-paths)
       ;; files added with --report-add
       (if added-paths
           (list (string-split added-paths #\:))
           '())))
     (examples
      (append-map
       (lambda (ff) (apply load-example-pairs ff))
       file-pairs))
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
                   (show-diff diff-unified expected-text actual-text)))
             (not is-success))))
       examples)))

  (unless (= 0 (length failures))
    (display "\nFailed examples:\n")
    (for-each
     (lambda (x) (display (car (first x))) (newline))
     failures))
  (newline)

  (display (colorize-string
            (string-append
             (number->string (length failures))
             " failures, "
             (number->string (length examples))
             " examples total\n")
            (if (= 0 (length failures)) 'GREEN 'RED)))

  (when (and (< 0 (length examples))
             (= 0 (length failures)))
    (display (colorize-string "ALL TESTS GREEN\n" 'GREEN))))
