\version "2.24.1"

\header {
  title = "Organ accompaniment"
  subtitle =
    \markup\with-url "https://archive.org/details/artofaccompanyin0000spri/page/176/mode/2up"
      "Max Springer: The art of accompanying plain chant (1908), p. 177"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 32)

\layout {
  \override VaticanaStaff.StaffSymbol.color = "black"
  \override VaticanaStaff.Clef.extra-offset = #'(0.3 . 0)

  % barlines only leak here from the accompaniment staffs -
  % for LilyPond chant divisiones are breathing signs, not barlines
  \override VaticanaStaff.BarLine.transparent = ##t

  \override VaticanaLyrics.LyricText.font-size = #-2

  \context {
    \PianoStaff
    \remove Span_bar_engraver
  }
  \context {
    \Staff
    \remove Time_signature_engraver
  }

  ragged-last = ##f
}

\paper {
  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 25)))
}

applyDurationsFrom =
#(define-music-function (source destination) (ly:music? ly:music?)
   (let ((durations '()))
     ;; collect durations from source
     (music-map
      (lambda (m)
        (when (eq? 'NoteEvent (ly:music-property m 'name))
          (set! durations (append durations (list (ly:music-property m 'duration)))))
        m)
      source)
     ;; apply them to destination
     (music-map
      (lambda (m)
        (when (eq? 'NoteEvent (ly:music-property m 'name))
          (ly:music-set-property! m 'duration (car durations))
          (set! durations (list-tail durations 1)))
        m)
      destination)
     destination))

chant =
  \gabc-vaticana
    "(c4) Ae(f)tér(f)ne(g) ré(h)rum(ixi) Cón(h)di(g)tor,(h) (;)
    nó(h)ctem(j) di(j)ém(j)que(g) qui(h) ré(ixi)gis,(h) (;)
    Et(g) tém(h)po(g)rum(h) das(g) tém(e)po(f)ra,(g) (;)
    Ut(g) ál(h)le(g)ves(e) fa(f)stí(g)di(f)um.(d) (::)"

accompanimentMelody = \relative c' {
  f8 f g a bes a g a4 \bar ","
  a8 c c c g a bes4 a \bar "|" \break
  g8 a g a g e f g4 \bar ","
  g8 a g e f g f d4 \bar "||"
}

global = {
  \key f \major
  \cadenzaOn
  \autoBeamOff
}

\score {
  <<
    \new VaticanaStaff {
      % voice alignment fixed by forcing durations from the accompaniment on the chant notes
      \applyDurationsFrom \accompanimentMelody \chant
    }

    \new PianoStaff <<
      \set PianoStaff.midiInstrument = #"church organ"

      \new Staff <<
        \new Voice {
          \global
          \stemUp
          \accompanimentMelody
        }
        \new Voice \relative c' {
          \global
          \stemDown
          r8 d4 f e2
          e8 f4 e f8 e4 f
          g8 e4 d c bes
          bes8 a4 bes d a
        }
      >>
      \new Staff <<
        \new Voice \relative c' {
          \global
          \stemUp
          \clef "bass"
          r8 bes4 c2~ c4
          c8 a4 g8[ c]~ c bes4 c
          c8~ c4 f,8[ g] a4 d,
          d8~ d4 g2 f4
        }
        \new Voice \relative c' {
          \global
          \stemDown
          r8 bes4 f c a
          a'8 f4 c f8 g4 f
          e8 c4 bes a g
          g8 f4 g bes d
        }
      >>
    >>
  >>
  % \midi {}
}
