(define-module (lilygabc)
  #:use-module (ice-9 textual-ports)
  #:use-module (lily)
  #:use-module (srfi srfi-1)
  #:use-module ((lilygabc gabc) #:prefix gabc:)
  #:use-module ((lilygabc pitch) #:prefix pitch:)
  #:use-module ((lilygabc util) #:prefix util:)
  #:use-module ((lilygabc lily modern) #:select (make-notes make-lyrics) #:prefix lilygabc:modern:)
  #:use-module ((lilygabc lily vaticana) #:prefix lilygabc:vaticana:))

;;
;; Tier 1 API: single LilyPond command to orchestrate everything
;; and transform gabc input to LilyPond music
;;

(define-public gabc
  (define-scheme-function
    (options input)
    ((symbol-key-alist? '()) string?)
    (let*
        ((parse-fn (if (eq? 'gly (assq-ref options 'parse-as)) parse-gly parse-gabc))
         (score (parse-fn input)))
      (set-notes-origin
       (modern-music options score)))))

(define-public gabc-file
  (define-scheme-function
    (options path)
    ((symbol-key-alist? '()) string?)
    (gabc
     options
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd

(define-public gabc-vaticana
  (define-scheme-function
    (options input)
    ((symbol-key-alist? '()) string?)
    (let*
        ((parse-fn (if (eq? 'gly (assq-ref options 'parse-as)) parse-gly parse-gabc))
         (score (parse-fn input)))
      (set-notes-origin
       (vaticana-music options score)))))

(define-public gabc-vaticana-file
  (define-scheme-function
    (options path)
    ((symbol-key-alist? '()) string?)
    (gabc-vaticana
     options
     (call-with-input-file path get-string-all)))) ; TODO: resolve path relative to the current lilypond file, not to cwd

(define-public gly
  (define-scheme-function
    (options input)
    ((symbol-key-alist? '()) string?)
    (gabc (acons 'parse-as 'gly options)
          input)))

(define-public gly-vaticana
  (define-scheme-function
    (options input)
    ((symbol-key-alist? '()) string?)
    (gabc-vaticana (acons 'parse-as 'gly options)
                   input)))



;;
;; Tier 2 API: individual operations
;;

;; string -> list representation of the parsed gabc.

(define-public parse-gabc
  (define-scheme-function
    (input)
    (string?)
    (gabc:parse input)))

(define-public parse-gly
  (define-scheme-function
    (input)
    (string?)
    (gabc:parse-gly input)))

;; parsed gabc -> complete music (voice + lyrics)

(define-public modern-music
  (define-scheme-function
    (options score)
    ((symbol-key-alist? '()) list?)
    (let*
        ((set-id (assq-ref options 'voice-id))
         (context-id (or set-id "uniqueContext0"))
         (requested-result (assq-ref options 'produce))
         (has-lyrics (any gabc:syl-has-lyrics? (util:flatten score))))
      (case requested-result
        ((notes) (modern-notes score))
        ((voice) (modern-voice set-id score))
        ((lyrics) (modern-lyrics set-id score))
        (else
         (if has-lyrics
             (make-simultaneous-music
              (list
               (modern-voice context-id score)
               (modern-lyrics context-id score)))
             (modern-notes score)))))))

(define-public vaticana-music
  (define-scheme-function
    (options score)
    ((symbol-key-alist? '()) list?)
    (let*
        ((set-id (assq-ref options 'voice-id))
         (context-id (or set-id "uniqueContext0"))
         (requested-result (assq-ref options 'produce))
         (has-lyrics (any gabc:syl-has-lyrics? (util:flatten score))))
      (case requested-result
        ((notes) (vaticana-notes score))
        ((voice) (vaticana-voice set-id score))
        ((lyrics) (vaticana-lyrics set-id score))
        (else
         (if has-lyrics
             (make-simultaneous-music
              (list
               (vaticana-voice context-id score)
               (vaticana-lyrics context-id score)))
             (vaticana-voice (if (or has-lyrics set-id) context-id #f) score)))))))

;; parsed gabc -> voice

(define-public modern-voice
  (define-scheme-function
    (context-id score)
    (string? list?)
    (context-spec-music
     (modern-notes score)
     'Voice
     context-id)))

(define-public vaticana-voice
  (define-scheme-function
    (context-id score)
    (util:string-or-false? list?)
    (let ((voice
           (context-spec-music
            (vaticana-notes score)
            'VaticanaVoice
            context-id)))
      (set! (ly:music-property voice 'create-new) #t)
      voice)))

;; parsed gabc -> lyrics

(define-public modern-lyrics
  (define-scheme-function
    (context-id score)
    (string? list?)
    (lilygabc:modern:make-lyrics score context-id 'Lyrics)))

(define-public vaticana-lyrics
  (define-scheme-function
    (context-id score)
    (string? list?)
    (lilygabc:modern:make-lyrics score context-id 'VaticanaLyrics)))

;; parsed gabc -> bare notes (not wrapped in a voice)

(define-public modern-notes
  (define-scheme-function
    (score)
    (list?)
    (lilygabc:modern:make-notes score)))

(define-public vaticana-notes
  (define-scheme-function
    (score)
    (list?)
    (lilygabc:vaticana:make-notes score)))



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
