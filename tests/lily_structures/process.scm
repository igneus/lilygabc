;; Process in parallel .ly files built by generate.scm
;; from the files specified as arguments

(use-modules
 (ice-9 getopt-long)
 (ice-9 string-fun)
 (srfi srfi-1))

(load "options.scm")

(define (process-file path)
  (let*
      ((out-path (string-replace-substring path ".ly" ".out"))
       (out-file (open-output-file out-path))
       (program "lilypond")
       (pid (spawn program (list program "--silent" path) #:output out-file)))
    `((input-path . ,path)
      (out-file . ,out-file)
      (pid . ,pid))))

(define (collect-result processing-info)
  (let*
      ((pid (assq-ref processing-info 'pid))
       (result (waitpid pid)) ; !!! wait for the process to end
       (intstatus (cdr result))
       (exit-status (status:exit-val intstatus)))
    (close-port (assq-ref processing-info 'out-file)) ; !!! close its stdout
    (append
     processing-info
     `((exit-status . ,exit-status)))))

(let*
    ((args (option-ref options '() '()))
     (merge-batches (option-ref options 'merge-batches #f))
     (source-files (if merge-batches '("all.ly") args))
     (paths
      (append-map
       (lambda (source-path)
         (let ((bn (basename source-path)))
           (list
            (string-replace-substring bn ".ly" "_expected.ly")
            (string-replace-substring bn ".ly" "_actual.ly"))))
       source-files))
     (processing
      (map process-file paths))
     (results
      (map collect-result processing))
     (errors
      (filter
       (lambda (x)
         (not (= 0 (assq-ref x 'exit-status))))
       results)))

  (when (< 0 (length errors))
    (begin
      (display "There were errors:")
      (for-each
       (lambda (info) (display (string-append " " (assq-ref info 'input-path))))
       errors)
      (exit EXIT_FAILURE))))
