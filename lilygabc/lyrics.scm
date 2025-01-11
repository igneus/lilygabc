;; processing lyrics

(define-module (lilygabc lyrics)
  #:use-module (ice-9 regex))

(define sp-tag-re (make-regexp "<sp>([^<]*)</sp>"))

(define special-chars
  '(("R/" . "℟")
    ("V/" . "℣")))

(define-public (expand-special-chars str)
  (let ((m (regexp-exec sp-tag-re str)))
    (if m
        (string-append
         (substring str 0 (match:start m))
         (or (assoc-ref special-chars (match:substring m 1))
             (match:substring m 0))
         (expand-special-chars (substring str (match:end m))))
        str)))

(define-public (remove-tags lyric-syllable)
  (regexp-substitute/global #f "<[^>]*>" lyric-syllable 'pre "" 'post))
