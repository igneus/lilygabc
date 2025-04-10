;; Implementation of generating modern notation.
;; Requires the LilyPond runtime.

(define-module (lilygabc lily modern)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (lily) ; LilyPond Scheme API
  #:use-module ((lily ly-syntax-constructors) #:select (context-create lyric-combine))
  #:use-module ((lilygabc episema) #:prefix episema:)
  #:use-module ((lilygabc gabc) #:prefix gabc:)
  #:use-module ((lilygabc lyrics) #:prefix lyrics:)
  #:use-module ((lilygabc pitch) #:prefix pitch:)
  #:use-module ((lilygabc util) #:prefix util:)
  #:use-module ((lilygabc lily lilypond-globals) #:prefix l:)
  #:use-module (lilygabc lily music-functions))

; mapping Gregorio divisiones -> LilyPond bars
(define divisiones-mapping
  `((","  . ,(l:bar "'"))
    (";"  . ,(l:bar ","))
    ("::" . ,(l:bar "||"))
    ("`"  . ,(l:breathe))))
(define default-bar (l:bar "|")) ; used for all not explicitly mapped

;; equivalent of gabc:syl-has-notes? operating on the results
;; of pitch:decorate-notes
(define-public (syl-has-decorated-notes? syllable)
  (find pitch:is-note-with-pitch? syllable))

(define-public (make-notes score)
  (make-sequential-music
   (util:flatten
    (let ((enhanced-score
           (pitch:decorate-notes
            (gabc:map-syllables expand-note-repetitions score)))
          (last-clef-was-flat #f))
      (map
       (lambda (syllable)
         (let* ((notes (filter pitch:is-note-with-pitch? syllable))
                (is-melisma (< 1 (length notes)))
                (first-note (and is-melisma (first notes)))
                (last-note (and is-melisma (last notes)))
                (items-with-episema-events
                 (episema:decorate-notes
                  pitch:is-note-with-pitch?
                  (lambda (x) (gabc:note-has-horizontal-episema? (second x)))
                  syllable)))
           (cond
            ((eq? '() syllable) ; void syllable rendered as invisible bar
             (list (l:bar "")))
            ((and (gabc:syl-has-lyrics? syllable)
                  (= 1 (length syllable))) ; the syllable has only lyrics, no renderable music elements
             (list (make-invisible-note)))
            (else
             (util:flatten
              (filter-map
               (lambda (item)
                 (match item
                   (('clef type line clef-is-flat)
                    (if (eq? last-clef-was-flat clef-is-flat)
                        #f
                        (begin
                          (set! last-clef-was-flat clef-is-flat)
                          (list (if clef-is-flat key-flat key-natural)))))
                   (('note-with-episema-events ('note-with-pitch note pitch) episema-events)
                    (list
                     (apply-note-features
                      note
                      (apply-episema-events
                       episema-events
                       (make-ly-note
                        (apply ly:make-pitch (list-tail pitch 1))
                        (if (gabc:note-has-punctum-mora? note)
                            (ly:make-duration 2 (gabc:note-punctum-mora-count note))
                            (ly:make-duration 2))
                        (if is-melisma
                            (cond ((eq? (second item) first-note) -1)
                                  ((eq? (second item) last-note) 1)
                                  (else #f))
                            #f))))))
                   (('divisio type)
                    (filter
                     values
                     (list
                      (if (and (gabc:syl-has-lyrics? syllable)
                               (not (syl-has-decorated-notes? syllable)))
                          ;; lyrics under a divisio are very common in gabc,
                          ;; but unsupported in LilyPond
                          (make-invisible-note)
                          #f)
                      (or (assoc-ref divisiones-mapping type)
                          default-bar))))
                   (('line-break type)
                    (list l:break))
                   (('unrecognized token)
                    (unrecognized-element-warning token (*location*))
                    #f)
                   (any #f)))
               items-with-episema-events))))))
       (util:flatten enhanced-score))))))

(define-public (make-ly-note pitch duration slur-direction)
  (apply
   make-music
   (append
    (list
     'NoteEvent
     'pitch pitch
     'duration duration)
    (if slur-direction
        (list 'articulations
              (list (make-music 'SlurEvent 'span-direction slur-direction)))
        '()))))

(define-public (apply-episema-events events ly-note)
  (if (null? events)
      ly-note
      (apply-episema-events
       (cdr events)
       (case (car events)
         ((open) (open-episema ly-note))
         (else (close-episema ly-note))))))

;; apply features of the gabc note
;; on a modern notation LilyPond note
(define (apply-note-features gabc-note ly-note)
  (let* ((virga-side (gabc:note-virga-side gabc-note))
         (tests-and-transformations
          `((,gabc:note-is-punctum-inclinatum? . ,tiny-note)
            (,gabc:note-is-diminutive? . ,teeny-note)
            (,gabc:note-is-debilis? . ,teeny-note)
            (,gabc:note-has-ictus? . ,(cut apply-articulation-down l:staccatissimo <>))
            (,gabc:note-is-virga? . ,(cut apply-virga virga-side <>))
            (,gabc:note-is-quilisma? . ,(cut apply-articulation l:prall <>))
            (,gabc:note-has-musica-ficta? . ,apply-musica-ficta))))
    (fold
     (lambda (x r)
       (match-let (((test . transformation) x))
         (if (test gabc-note)
             (transformation r)
             r)))
     ly-note
     tests-and-transformations)))

(define-public (expand-note-repetitions syllable)
  (append-map
   (lambda (item)
     (if (gabc:is-note? item)
         (let ((num (or (gabc:note-repetitions item) 1)))
           (map (lambda (i) (list-copy item)) (iota num)))
         (list item)))
   syllable))

(define-public (make-lyrics options words context-id lyrics-type)
  (let*
      ((formatting-functions
        (if (null? options)
            standard-markup-formatting-fns
            (build-markup-formatting-fns (append options lilygabc-global-settings))))
       (custom-special-chars
        (or (assoc-ref options 'special-characters) '()))
       (expand-lyrics
        (lyrics:expander #:custom-special-chars custom-special-chars)))
    (context-create
     lyrics-type '() '()
     (lyric-combine
      context-id
      'Voice
      (make-sequential-music
       (append
        (append-map
         (lambda (word)
           (let ((lyrics
                  (filter-map
                   (lambda (syllable)
                     (cond
                      ((gabc:syl-has-lyrics? syllable)
                       (first syllable))
                      ((gabc:syl-has-notes? syllable)
                       '(lyrics ""))
                      (else #f)))
                   word)))
             (map
              (lambda (lyr)
                (let* ((lyr-str
                        (apply-lyrics-formatting
                         formatting-functions
                         (expand-lyrics (second lyr))))
                       (lyric
                        (make-lyric-event
                         lyr-str
                         (ly:make-duration 2))))
                  (when (and (> (length word) 1)
                             (not (eq? lyr (last lyrics))))
                    (set! (ly:music-property lyric 'articulations)
                          (list (make-music 'HyphenEvent))))
                  lyric))
              lyrics)))
         words)))))))

;; accepts the data structure produced by lyrics:expand,
;; produces valid value for the LyricEvent.text property
(define (apply-lyrics-formatting formatting-functions parsed-syllable)
  (cond
   ((null? parsed-syllable)
    "")
   ((and (= 1 (length parsed-syllable))
         (string? (first parsed-syllable)))
    (first parsed-syllable)) ; no formatting
   (else
    (markup
      (if (< 1 (length parsed-syllable))
          (make-concat-markup (map (cut build-lyrics-markup formatting-functions <>) parsed-syllable))
          (build-lyrics-markup formatting-functions (first parsed-syllable)))))))

;; accepts an alist with keys and value types matching lilygabc-global-settings,
;; builds an alist mapping format symbols produced by lyrics:expand (e.g. 'bold)
;; to the corresponding LilyPond functions (make-bold-markup)
(define (build-markup-formatting-fns options)
  (append
   (options-to-formatting-fns options)
   standard-markup-formatting-fns))

;; accepts an alist with keys and value types matching lilygabc-global-settings,
;; builds the markup formatting functions which depend on customizable settings
(define (options-to-formatting-fns options)
  `((color . ,(cut make-with-color-markup (assoc-ref options 'c-tag-color) <>))
    (verbatim
     . ,(lambda (tag-body)
          (case (assoc-ref options 'verbatim-tag)
            ((ignore) "")
            ((as-lilypond) (ly:parse-string-expression (ly:parser-clone) tag-body)) ; TODO add `filename` and `line` optional arguments - is it possible to extract the values from (*location*)?
            (else (make-typewriter-markup tag-body)))))))

(define standard-markup-formatting-fns
  (append
   ;; custom functions
   (options-to-formatting-fns lilygabc-global-settings)
   ;; standard LilyPond make-*-markup functions
   (filter-map
    (lambda (tagname-sym-pair)
      (let* ((sym (cdr tagname-sym-pair))
             (func-name (string->symbol (string-append "make-" (symbol->string sym) "-markup"))))
        (and (defined? func-name)
             (cons sym (primitive-eval func-name)))))
    lyrics:formatting-tags)))

;; recursively translates to markup an element
;; of the data structure produced by lyrics:expand
(define (build-lyrics-markup formatting-functions arg)
  (cond
   ((string? arg) arg)
   ((string? (car arg)) (car arg))
   (else
    (let ((markup-func (assq-ref formatting-functions (car arg))))
      (markup-func (build-lyrics-markup formatting-functions (cdr arg)))))))

(define-public (unrecognized-element-warning str location)
  (let ((loc-list (ly:input-file-line-char-column (*location*))))
    (ly:warning-located
     (format #f "~a:~a" (first loc-list) (second loc-list))
     (format #f "Unrecognized gabc music element: ~a" str))))
