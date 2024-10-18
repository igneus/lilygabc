;; Operates on a visual test file.
;; Identifies test examples, produces two LilyPond files:
;;  1. hand-coded expected results
;;  2. examples rendered by lilygabc
;; Both files, instead of rendering music notation, dump
;; LilyPond data structures.

(use-modules
 (ice-9 getopt-long)
 (ice-9 rdelim)
 (ice-9 regex)
 (ice-9 textual-ports))

(load "options.scm")

(define (transform-example str)
  (let*
      ((search "\\score")
       (score-content
        (substring str
                   (+ (string-contains str search)
                      (string-length search))))
       (lily-code (string-append "\\void \\displayLilyMusic" score-content))
       (scheme-code (string-append "\\void \\displayMusic" score-content)))
    (cond
     ((option-ref options 'lily-only #f)
      lily-code)
     ((option-ref options 'scheme-only #f)
      scheme-code)
     (else
      (string-append
       "#(display \"% LilyPond:\")\n"
       lily-code
       "\n"
       "#(display \"% Scheme:\")\n"
       scheme-code)))))

(define (suffix-filename suffix input-path)
  (let ((bn (basename input-path)))
    (string-append
     (substring bn 0 (string-contains bn ".ly"))
     suffix
     ".ly")))

(define (generate filename setup-both setup-expected setup-actual)
  (let*
      ((example-pattern (option-ref options 'example #f))
       (file (open-input-file filename))
       (fw-expected (open-output-file (suffix-filename "_expected" filename)))
       (fw-actual (open-output-file (suffix-filename "_actual" filename)))
       (write-both (lambda (str)
                     (put-string fw-expected str)
                     (put-string fw-actual str)))
       (line-i 0))

    (write-both "\\version \"2.24.0\"\n")
    (write-both setup-both)
    (put-string fw-expected setup-expected)
    (put-string fw-actual setup-actual)

    (do ((line (read-line file) (read-line file))) ((eof-object? line))
      (set! line-i (+ 1 line-i))

      (when (string-match "^\\s*% @test" line)
        (let ((example-name (string-append filename ":" (number->string line-i))))
          (when (or (eq? #f example-pattern)
                    (string-contains example-name example-pattern))
            (write-both (string-append "#(display \"% test " example-name "\\n\")\n"))

            (put-string fw-expected (transform-example (read-line file)))
            (put-string fw-actual (transform-example (read-line file)))
            (write-both "\n")

            (set! line-i (+ 2 line-i))))))

    (for-each close-port (list file fw-expected fw-actual))))



(generate "../visual/test.ly"
          ""
          ""
          "\\include \"../../lilygabc.ily\"\n")

(generate "../visual/vaticana_test.ly"
          "\\include \"gregorian.ly\"\n"
          ""
          "\\include \"../../lilygabc.ily\"\n")
