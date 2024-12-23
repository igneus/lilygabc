\version "2.24.1"

\header {
  title = "Chant with Solfege"
  tagline = ##f
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 30)

\layout {
  \override VaticanaStaff.StaffSymbol.color = "black"
  \override VaticanaLyrics.LyricText.font-size = #-2

  ragged-last = ##f
}

\paper {
  #(set-paper-size "a5")

  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 21)))

  markup-system-spacing.basic-distance = #1
}

lygabc =
  % music from https://gregobase.selapa.net/chant.php?id=8653
  \lilygabc-parse-gabc
    "(c4)Re(h)rum(h') De(h)us(h) te(h)nax(h') vi(g)gor,(g'_) (,)
    Im(h)mó(h')tus(h) in(h) te(g) pér(h')ma(h)nens,(h.) (;z)
    Lu(h)cis(h') di(h)úr(g)næ(f) tém(g')po(f)ra(e'_) (,)
    Suc(f)cés(g')si(g)bus(g) de(f)tér(h')mi(g)nans.(g.) (::)"

\score {
  <<
  \lilygabc-vaticana-voice "voiceid" \lygabc

  \new NoteNames {
    \set printNotesLanguage = "français"
    \override NoteName.font-size = #-4

    \lilygabc-vaticana-notes \lygabc
  }

  \lilygabc-vaticana-lyrics "voiceid" \lygabc
  >>
}
