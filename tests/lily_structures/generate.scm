;; Operates on a visual test file.
;; Identifies test examples, produces two LilyPond files:
;;  1. hand-coded expected results
;;  2. examples rendered by lilygabc
;; Both files, instead of rendering music notation, dump
;; LilyPond data structures.

(use-modules
 (ice-9 rdelim)
 (ice-9 regex)
 (ice-9 textual-ports))

(define (transform-example str)
  (let ((search "\\score"))
    (string-append
     "#(display \"% LilyPond:\")\n"
     "\\void \\displayLilyMusic"
     (substring str
                (+ (string-contains str search)
                   (string-length search)))
     "\n"
     "#(display \"% Scheme:\")\n"
     "\\void \\displayMusic"
     (substring str
                (+ (string-contains str search)
                   (string-length search))))))

(let*
    ((filename "../visual/test.ly")
     (file (open-input-file filename))
     (fw-expected (open-output-file "expected.ly"))
     (fw-actual (open-output-file "actual.ly"))
     (write-both (lambda (str)
                   (put-string fw-expected str)
                   (put-string fw-actual str)))
     (line-i 0))

  (write-both "\\version \"2.24.0\"\n")
  (put-string fw-actual "\\include \"../../lilygabc.ily\"\n")

  (do ((line (read-line file) (read-line file))) ((eof-object? line))
    (set! line-i (+ 1 line-i))

    (when (string-match "^\\s*% test" line)
      (write-both (string-append "#(display \"% test " filename ":" (number->string line-i) "\\n\")\n"))

      (put-string fw-expected (transform-example (read-line file)))
      (put-string fw-actual (transform-example (read-line file)))
      (write-both "\n")

      (set! line-i (+ 2 line-i))))

  (for-each close-port (list file fw-expected fw-actual)))
