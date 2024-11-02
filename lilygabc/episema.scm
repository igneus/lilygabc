(define-module (lilygabc episema)
  #:use-module (srfi srfi-1)
  #:use-module (lilygabc gabc))

;; Accepts a list of music items (parts of a music syllable),
;; returns a list containing for each item a list of episema events.
(define-public (episema-events is-note-with-episema? items)
  (let ((in-episema #f))
    (pair-fold
     (lambda (sublist r)
       (let ((i (first sublist))
             (next (and (< 1 (length sublist)) (second sublist))))
         (append
          r
          (list
           (if in-episema
               (if (and next (is-note-with-episema? next))
                   '()
                   (begin
                     (set! in-episema #f)
                     '(close)))
               (if (and (is-note-with-episema? i))
                   (if (and next (is-note-with-episema? next))
                       (begin
                         (set! in-episema #t)
                         '(open))
                       '(open close))
                   '()))))))
     '()
     items)))

;; Runs episema-events with a default episema detection function,
;; suitable for vanilla data produced by gabc:parse
(define-public (episema-events-default items)
  (episema-events
   (lambda (i) (and (is-note? i) (note-has-horizontal-episema? i)))
   items))

(define (is-note? item)
  (and (< 0 (length item))
       (eq? 'note (first item))))
