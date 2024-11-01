(use-modules
 (ice-9 textual-ports)
 ((lilygabc gabc) #:prefix gabc:)
 ((lilygabc pitch) #:prefix pitch:)
 ((lilygabc util) #:prefix util:)
 ((lilygabc lily modern) #:select (make-notes make-lyrics) #:prefix lilygabc:modern:)
 ((lilygabc lily vaticana) #:prefix lilygabc:vaticana:))

;;
;; Tier 1 API: single LilyPond command to orchestrate everything
;; and transform gabc input to LilyPond music
;;

(define gabc
  (define-scheme-function
    (options input)
    ((symbol-key-alist? '()) string?)
    (let*
        ((parse-fn (if (eq? 'gly (assq-ref options 'parse-as)) lilygabc-parse-gly lilygabc-parse-gabc))
         (score (parse-fn input)))
      (set-notes-origin
       (lilygabc-modern-music options score)))))

(define gabc-file
  (define-scheme-function
    (options path)
    ((symbol-key-alist? '()) string?)
    (gabc
     options
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd

(define gabc-vaticana
  (define-scheme-function
    (options input)
    ((symbol-key-alist? '()) string?)
    (let*
        ((parse-fn (if (eq? 'gly (assq-ref options 'parse-as)) lilygabc-parse-gly lilygabc-parse-gabc))
         (score (parse-fn input)))
      (set-notes-origin
       (lilygabc-vaticana-music options score)))))

(define gabc-vaticana-file
  (define-scheme-function
    (options path)
    ((symbol-key-alist? '()) string?)
    (gabc-vaticana
     options
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd



;;
;; Tier 2 API: individual operations
;;

;; string -> list representation of the parsed gabc.

(define lilygabc-parse-gabc
  (define-scheme-function
    (input)
    (string?)
    (gabc:parse input)))

(define lilygabc-parse-gly
  (define-scheme-function
    (input)
    (string?)
    (gabc:parse-gly input)))

;; parsed gabc -> complete music (voice + lyrics)

(define lilygabc-modern-music
  (define-scheme-function
    (options score)
    ((symbol-key-alist? '()) list?)
    (let*
        ((context-id (or (assq-ref options 'voice-id) "uniqueContext0"))
         (has-lyrics (any gabc:syl-has-lyrics? (util:flatten score))))
      (if has-lyrics
          (make-simultaneous-music
           (list
            (lilygabc-modern-voice context-id score)
            (lilygabc-modern-lyrics context-id score)))
          (lilygabc-modern-notes score)))))

(define lilygabc-vaticana-music
  (define-scheme-function
    (options score)
    ((symbol-key-alist? '()) list?)
    (let*
        ((set-id (assq-ref options 'voice-id))
         (context-id (or set-id "uniqueContext0"))
         (has-lyrics (any gabc:syl-has-lyrics? (util:flatten score))))
      (if has-lyrics
          (make-simultaneous-music
           (list
            (lilygabc-vaticana-voice context-id score)
            (lilygabc-vaticana-lyrics context-id score)))
          (lilygabc-vaticana-voice (if (or has-lyrics set-id) context-id #f) score)))))

;; parsed gabc -> voice

(define lilygabc-modern-voice
  (define-scheme-function
    (context-id score)
    (string? list?)
    (context-spec-music
     (lilygabc-modern-notes score)
     'Voice
     context-id)))

(define lilygabc-vaticana-voice
  (define-scheme-function
    (context-id score)
    (util:string-or-false? list?)
    (let ((voice
           (context-spec-music
            (lilygabc-vaticana-notes score)
            'VaticanaVoice
            context-id)))
      (set! (ly:music-property voice 'create-new) #t)
      voice)))

;; parsed gabc -> lyrics

(define lilygabc-modern-lyrics
  (define-scheme-function
    (context-id score)
    (string? list?)
    (lilygabc:modern:make-lyrics score context-id 'Lyrics)))

(define lilygabc-vaticana-lyrics
  (define-scheme-function
    (context-id score)
    (string? list?)
    (lilygabc:modern:make-lyrics score context-id 'VaticanaLyrics)))

;; parsed gabc -> bare notes (not wrapped in a voice)

(define lilygabc-modern-notes
  (define-scheme-function
    (score)
    (list?)
    (lilygabc:modern:make-notes (pitch:decorate-notes score))))

(define lilygabc-vaticana-notes
  (define-scheme-function
    (score)
    (list?)
    (lilygabc:vaticana:make-notes
     (pitch:decorate-notes
      score
      #:base-octave -1)))) ; LilyPond treats the chant c clef as middle c



;; private

(define (set-notes-origin music)
  (music-map
   (lambda (m)
     (let ((name (ly:music-property m 'name)))
       (if (or (eq? 'NoteEvent name)
               (eq? 'LyricEvent name))
           (ly:set-origin! m (*location*))
           m)))
   music))
