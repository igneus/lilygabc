;; Generates the Gregorio vs. lilygabc side-by-side comparison

(use-modules
 (ice-9 getopt-long)
 (ice-9 rdelim)
 (ice-9 textual-ports)
 (srfi srfi-26))

(define header "\\documentclass[a4paper, 12pt]{article}
\\usepackage[left=1.5cm, right=1.5cm, top=3cm, bottom=3cm]{geometry}

\\usepackage{lyluatex}
\\usepackage{gregoriotex}

% This is a rather humbling workaround:
% the modern notation examples produce crazy vertical margins when set
% directly in a minipage, but are well-behaved in a framed/shaded
% environment. So we set them in a shaded environment with white
% (i.e. invisible) shade.
% TODO find a cleaner solution
\\usepackage{framed}
\\usepackage[usenames]{xcolor}
\\colorlet{shadecolor}{white}

\\title{lilygabc visual tests: comparison with gregorio}

\\begin{document}
\\maketitle")

(define item-template '("% " " ----------
\\noindent
\\begin{minipage}[t]{0.25\\textwidth}
  % gabc as text
  \\mbox{}\\vspace*{3.5mm}\\begin{verbatim}" "\\end{verbatim}
\\end{minipage}
\\begin{minipage}[t]{0.25\\textwidth}
  % Gregorio
  \\mbox{}\\vspace*{2.2mm}\\gabcsnippet{" "}
\\end{minipage}
\\begin{minipage}[t]{0.25\\textwidth}
  % lilygabc square notation
  \\mbox{}\\vspace*{6.45mm}\\begin{lilypond}
    \\include \"gregorian.ly\"
    \\include \"../../lilygabc.ily\"
    #(set-global-staff-size 33)
    \\score {
      \\gabc-vaticana \"" "\"
    }
  \\end{lilypond}
\\end{minipage}
\\begin{minipage}[t]{0.25\\textwidth}
  % lilygabc modern notation
  \\begin{shaded*}\\begin{lilypond}
    \\include \"../../lilygabc.ily\"
    \\score {
      \\gabc \"" "\"
      \\layout { \\lilygabcModernGregorianStemlessLayout }
    }
  \\end{lilypond}\\end{shaded*}
\\end{minipage}"))

(define footer "\\end{document}")

(define (make-title str)
  (string-append "\\section*{" str "}\n"))

(define (make-item str)
  (string-join item-template str))

(define (process-input-from-port port output-port)
  (do ((line (read-line port) (read-line port))) ((eof-object? line))
    (cond
     ((string-prefix? "#" line)) ; do nothing
     ((= 0 (string-length (string-trim-both line)))) ; do nothing
     ((string-prefix? "=" line)
      (display (make-title (string-trim-both (substring line 1))) output-port)
      (newline output-port))
     (else
      (display (make-item line) output-port)
      (newline output-port) (newline output-port)))))



;; command line options
(define option-spec
  ;; --output-file -o FILE : write to a specified file instead of standard output
  '((output-file (single-char #\o) (value #t))
    ;; --compile -c : run lualatex
    (compile (single-char #\c) (value #f))))

(define options (getopt-long (command-line) option-spec #:stop-at-first-non-option #t))

(let* ((input-files (option-ref options '() '()))
       (output-file (option-ref options 'output-file #f))
       (fw (if output-file
              (open-output-file output-file)
              (current-output-port)))
       (compile (option-ref options 'compile #f)))
  (display "% Generated by generate.scm" fw)
  (newline fw)
  (display header fw)
  (newline fw)
  (if (= 0 (length input-files))
      (process-input-from-port (current-input-port) fw)
      (for-each
       (cut call-with-input-file <>
            (cut process-input-from-port <> fw))
       input-files))
  (display footer fw)
  (newline fw)
  (when output-file
    (close-port fw))
  (when (and compile output-file)
    (system (string-append "lualatex -shell-escape " output-file))))
