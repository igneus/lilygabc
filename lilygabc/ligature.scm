(define-module (lilygabc ligature)
  #:use-module (srfi srfi-1)
  #:use-module ((lilygabc gabc) #:prefix gabc:))

;; Logic specific to square notation:
;; determine where ligature brackets must be placed
;; for the notes to be correctly rendered.
(define-public (add-ligatures syllable)
  (if (= 0 (length syllable))
      syllable
      (inner syllable #f)))

;; TODO there must be a way to make the function concise and easy to read
(define (inner lst in-ligature)
  (let ((head (car lst))
        (tail (cdr lst)))
    (cond
     ((= 1 (length lst))
      (cond
       (in-ligature
        (append lst '((ligature close))))
       ((and (gabc:is-note? head)
             (gabc:note-has-special-note-head? head))
        `((ligature open) ,head (ligature close)))
       (else lst)))
     ((and (not in-ligature)
           (gabc:is-note? head)
           (or (begins-with-a-note? tail)
               (gabc:note-has-special-note-head? head)))
      (append
       (list '(ligature open) head)
       (if (begins-with-a-note? tail)
           '()
           '((ligature close)))
       (inner tail (begins-with-a-note? tail))))
     ((and in-ligature
           (breaks-ligature? head))
      (append
       (list '(ligature close) head)
       (inner tail #f)))
     (else
      (cons head (inner tail in-ligature))))))

(define (begins-with-a-note? lst)
  (and (< 0 (length lst))
       (let ((head (car lst)))
         (or (gabc:is-note? head)
             (and (not (breaks-ligature? head))
                  (begins-with-a-note? (cdr lst)))))))

(define (is-space? item)
  (eq? 'space (car item)))

(define (breaks-ligature? item)
  (eq? 'divisio (car item)))
