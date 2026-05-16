;; Operates on a visual test file.
;; Identifies test examples, produces two LilyPond files:
;;  1. hand-coded expected results
;;  2. examples rendered by lilygabc
;; Both files, instead of rendering music notation, dump
;; LilyPond data structures.

(use-modules
 (ice-9 getopt-long)
 (ice-9 rdelim)
 (ice-9 receive)
 (ice-9 regex)
 (ice-9 string-fun)
 (ice-9 textual-ports))

(load "options.scm")

(define (extract-settings fd)
  (let* ((version #f)
         (include-expected '())
         (include-actual '())
         (settings-finished?
          (lambda (line)
            (and (string-contains line "\\")
                 (not (or (string-contains line "\\version")
                          (string-contains line "\\include")))))))
    (do ((line (read-line fd) (read-line fd))) ((or (eof-object? fd) (settings-finished? line)))
      (when (string-contains line "\\version")
        (set! version line))
      (when (string-contains line "\\include")
        (when (or (not (string-contains line "@"))
                  (string-contains line "@for-expected"))
          (set! include-expected (cons line include-expected)))
        (when (or (not (string-contains line "@"))
                  (string-contains line "@for-actual"))
          (set! include-actual (cons line include-actual)))))
    (values
     version
     (reverse include-expected)
     (reverse include-actual))))

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
  (string-replace-substring
   (basename input-path)
   ".ly"
   (string-append suffix ".ly")))

(define (generate filename)
  (let*
      ((example-pattern (option-ref options 'example #f))
       (file (open-input-file filename))
       (fw-expected (open-output-file (suffix-filename "_expected" filename)))
       (fw-actual (open-output-file (suffix-filename "_actual" filename)))
       (write-both (lambda (str)
                     (put-string fw-expected str)
                     (put-string fw-actual str)))
       (line-i 0))
    (receive (version include-expected include-actual)
        (extract-settings file)
      (write-both (or version "\\version \"2.24.0\""))
      (write-both "\n")
      (for-each (lambda (s) (put-string fw-expected (string-append s "\n")))
                include-expected)
      (for-each (lambda (s) (put-string fw-actual (string-append s "\n")))
                include-actual))

    (do ((line (read-line file) (read-line file))) ((eof-object? line))
      (set! line-i (+ 1 line-i))

      (let ((match (string-match "^\\s*% @test(.*?)$" line)))
        (when match
          (let* ((label (string-trim-both (match:substring match 1)))
                 (example-name (string-append
                                (if (< 0 (string-length label))
                                    (string-append label " ")
                                    "")
                                filename ":" (number->string line-i))))
            (when (or (eq? #f example-pattern)
                      (string-contains example-name example-pattern))
              (write-both (string-append "#(display \"% test " example-name "\\n\")\n"))

              (put-string fw-expected (transform-example (read-line file)))
              (put-string fw-actual (transform-example (read-line file)))
              (write-both "\n")

              (set! line-i (+ 2 line-i)))))))

    (for-each close-port (list file fw-expected fw-actual))))



(let ((args (option-ref options '() '())))
  (map generate args))
