(define-module (lilygabc ligature)
  #:use-module ((lilygabc gabc) #:prefix gabc:))

;; Logic specific to square notation:
;; determine where ligature brackets must be placed
;; for the notes to be correctly rendered.
(define-public (add-ligatures syllable)
  (if (= 0 (length syllable))
      syllable
      (not-in-ligature syllable)))

;; Music items we enrich the parsed gabc with
(define ligature-open  '(ligature open))
(define ligature-close '(ligature close))

(define (in-ligature lst)
  (let ((head (car lst))
        (tail (cdr lst)))
    (cond
     ((= 1 (length lst))
      (append lst (list ligature-close)))
     ((breaks-ligature? head)
      (append
       (list ligature-close head)
       (not-in-ligature tail)))
     (else
      (cons head (in-ligature tail))))))

(define (not-in-ligature lst)
  (let ((head (car lst))
        (tail (cdr lst)))
    (cond
     ((and (gabc:is-note? head)
           (or (begins-with-a-note? tail)
               (gabc:note-has-special-note-head? head)))
      (cons ligature-open (in-ligature lst)))
     ((= 1 (length lst))
      lst)
     (else
      (cons head (not-in-ligature tail))))))

(define (begins-with-a-note? lst)
  (and (< 0 (length lst))
       (let ((head (car lst)))
         (or (gabc:is-note? head)
             (and (not (breaks-ligature? head))
                  (begins-with-a-note? (cdr lst)))))))

(define (breaks-ligature? item)
  (eq? 'divisio (car item)))
