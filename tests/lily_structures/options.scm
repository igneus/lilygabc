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
    (scheme-only (single-char #\S) (value #f))))

(define options (getopt-long (command-line) option-spec))
