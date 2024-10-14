(use-modules
 (ice-9 match)
 (ice-9 regex)
 (ice-9 textual-ports)
 (srfi srfi-26)
 ((lilygabc gabc) #:prefix gabc:)
 ((lilygabc pitch) #:prefix pitch:)
 ((lilygabc util) #:prefix util:))

(define gabc-vaticana
  (define-scheme-function
    (input)
    (string?)
    (let*
        ((words (pitch:decorate-notes (gabc:parse input)))
         (syllable-items (util:flatten (util:flatten words))))
      (make-music
       'ContextSpeccedMusic
       'create-new
       #t
       'property-operations
       '()
       'context-type
       'VaticanaVoice
       'element
       (make-music
        'SequentialMusic
        'elements
        (append
         (list
          (make-music
           'ContextSpeccedMusic
           'context-type
           'Staff
           'element
           (make-music
            'SequentialMusic
            'elements
            (list
             (make-music 'PropertySet 'value "clefs.vaticana.do" 'symbol 'clefGlyph)
             (make-music 'PropertySet 'value 3 'symbol 'middleCClefPosition)
             (make-music 'PropertySet 'value 3 'symbol 'clefPosition)
             (make-music 'PropertySet 'value 0 'symbol 'clefTransposition)
             (make-music 'PropertySet 'value 'default 'symbol 'clefTranspositionStyle)
             (make-music 'ApplyContext 'procedure ly:set-middle-C!)))))
         (filter-map
          (lambda (item)
            (match item
              (('note-with-pitch note pitch)
               ;; shift the pitch an octave lower:
               ;; LilyPond treats the chant c clef as denoting middle c
               (let ((vaticana-pitch (cons (- (second pitch) 1) (list-tail pitch 2))))
                 (make-ly-note
                  (apply ly:make-pitch vaticana-pitch)
                  (ly:make-duration 2)
                  #f)))
              (any #f)))
            syllable-items)))))))

(define gabc-vaticana-file
  (define-scheme-function
    (path)
    (string?)
    (gabc-vaticana
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd
