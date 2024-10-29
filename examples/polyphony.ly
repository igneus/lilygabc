\version "2.24.1"

\header {
  title = "Polyphony"
  subtitle = \markup{Amen ad hymnum de S. Ludmilla \italic{Lux vera lucis radium}}
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 32)

\layout {
  \override VaticanaStaff.StaffSymbol.color = "black"
  \override VaticanaStaff.LedgerLineSpanner.color = "black"
  \override VaticanaStaff.Clef.extra-offset = #'(0.3 . 0)

  \override VaticanaLyrics.LyricText.font-size = #-2
  \override VaticanaLyrics.VerticalAxisGroup.staff-affinity = #CENTER

  ragged-last = ##f
}

\paper {
  #(set-paper-size "a5")

  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 25)))

  markup-markup-spacing.basic-distance = #9
  markup-system-spacing.basic-distance = #1
}

% Halve the duration of every NoteEvent that is part of a ligature
% (voice alignment hack that works well for this particular piece)
halveNotesInLigatures =
#(define-music-function (music) (ly:music?)
   (let ((in-ligature #f))
     (music-map
      (lambda (m)
        (case (ly:music-property m 'name)
          ((LigatureEvent)
           (set! in-ligature (= -1 (ly:music-property m 'span-direction))))
          ((NoteEvent)
           (when in-ligature
             (ly:music-compress m 1/2))))
        m)
      music)))

\markup\tiny\fill-line{
  ""
  \with-url
    "https://new.manuscriptorium.com/apis/resolver-api/cs/browser/default/detail?url=https://collectiones.manuscriptorium.com/assorted/AIPDIG/NKCR__/7/AIPDIG-NKCR__XIV_G_46____2SSE2F7-cs/&imageId=https://imagines.manuscriptorium.com/loris/AIPDIG-NKCR__XIV_G_46____2SSE2F7-cs/ID0102r"
    "CZ-Pu XIV G 46, f. 102r"
}

\score {
  <<
    \halveNotesInLigatures \gabc-vaticana #'((voice-id . "a") (parse-as . gly))
      "c4 k j hg f g f g  h : h j k  j hg fe f d :z
          d f d  e f g fe d : d f hi j k  ji j k ::"

    \new VaticanaLyrics \lyricsto "a" { \repeat unfold 16 { A -- men } }

    \halveNotesInLigatures \gabc-vaticana #'((parse-as . gly))
      "c4 d c d f g fe f d : b d b  d b d  e f :
          h j h i j k  j k : k j hg f g fe f d ::"
  >>
  \layout {}
  % \midi { \tempo 4 = 100 } % Uncomment to generate MIDI
}
