\version "2.24.1"

% Transform music loaded from gabc before rendering -
% either transform the parsed gabc (before translating to LilyPond)
% or the LilyPond data structures.

\header {
  title = "Transforming chant contents"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 30)

\layout {
  \override VaticanaStaff.StaffSymbol.color = "black"
  \override VaticanaLyrics.LyricText.font-size = #-2
}

\paper {
  #(set-paper-size "a5")

  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 21)))
}

parsed =
  \lilygabc-parse-gabc
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ja.(e.) (::)"

\markup\justify{
  Gabc contents rendered unchanged:
}
\score {
  \lilygabc-vaticana-music \parsed
}



% Transform parsed gabc -----------------------

\markup\justify{
  Remove rhythmic signs and phonetic J
  by transforming lilygabc data structures
  before translating them to LilyPond.
}

#(use-modules
  (ice-9 string-fun)
  (lilygabc gabc))

#(define (gabc-rhythmic-sign? char)
   (member char '(#\' #\. #\_)))

#(define (remove-phonetic-j char)
   (case char
     ((#\j) #\i)
     ((#\J) #\I)
     (else char)))

% Note: for the time being the parsed gabc data structure
% is not considered part of lilygabc public API and can be changed
% without prior notice.
#(define (gabc-remove-rhythmic-signs parsed-gabc)
   (map-syl-elements
    (lambda (i)
      (cond
       ;; remove rhythmic signs
       ((and (is-note? i) (> (length i) 2))
        (let ((j (list-copy i))
              (additional (third i)))
          (list-set! j 2 (string-filter (negate gabc-rhythmic-sign?) additional))
          j))
       ;; transform phonetic j to i
       ((equal? 'lyrics (first i))
        (list 'lyrics
              (string-map remove-phonetic-j (second i))))
       (else i)))
    parsed-gabc))

\score {
  \lilygabc-vaticana-music #(gabc-remove-rhythmic-signs parsed)
}



% Transform LilyPond -----------------------

\markup\justify{
  Similar effect can be achieved by transforming LilyPond data structures
  and/or adjusting score layout settings.
}

#(define (transform-phonetic-j-lyrics m)
   (when (equal? 'LyricEvent (ly:music-property m 'name))
     (let ((text (ly:music-property m 'text)))
       (ly:music-set-property! m 'text (string-map remove-phonetic-j text))))
   #f)

\score {
  #(map-some-music transform-phonetic-j-lyrics (lilygabc-vaticana-music parsed))
  \layout {
    \context {
      \VaticanaVoice
      \remove "Episema_engraver"
      \remove "Dots_engraver"
      \remove "Script_engraver" % TODO shouldn't this remove ictus?
    }
  }
}
