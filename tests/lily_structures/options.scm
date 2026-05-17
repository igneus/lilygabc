;; generate.scm and report.scm share a single definition
;; of command line
;; options, so that the wrapping shell script can pass
;; all options to both and not bother about which script
;; handles which option.

;; command line options
(define option-spec
  ;; --example -e STR : run only examples whose names contain the specified string
  '((example (single-char #\e) (value #t))
    ;; --lily-only -L : only check dumped LilyPond code (by default both LilyPond and Scheme code are checked)
    (lily-only (single-char #\L) (value #f))
    ;; --scheme-only -S : only check dumped Scheme code
    (scheme-only (single-char #\S) (value #f))
    ;; --unified -U NUM : output NUM (default 3) lines of unified context in diff
    (unified (single-char #\U) (value #t))
    ;; --report-add -r EXPECTED:ACTUAL : add pair of files to the final report only (without doing anything about them in the generating or processing phase)
    (report-add (single-char #\r) (value #t))
    ;; --merge-batches -m : process all examples in a single batch, thus running only two LilyPond processes instead of two processes per argument
    (merge-batches (single-char #\m) (value #f))))

(define options (getopt-long (command-line) option-spec))
