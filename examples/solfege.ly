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
  % music from https://gregobase.selapa.net/chant.php?id=2539
  \lilygabc-parse-gabc
    "(f3) Ut(e) que(f')ant(h) la(fg)xis(f.) (,)
    re(f')so(f)ná(e')re(f) fi(g)bris(g.) (;)
    Mi(g!hwi)ra(g) ge(f)stó(ge)rum(f.) (,z)
    fá(h')mu(i)li(j) tu(i)ó(hf)rum,(f.) (:)
    Sol(iji)ve(hg) pol(h)lú(i)ti(f.) (,)
    lá(j')bi(i)i(j) re(hi)á(j)tum,(j.) (,z)
    San(ih)cte(gf) Jo(e)án(g)nes.(f.) (::)"

\score {
  <<
  \lilygabc-vaticana-voice "voiceid" \lygabc

  \new NoteNames {
    \set printNotesLanguage = "italiano"
    \override NoteName.font-size = #-4

    \lilygabc-vaticana-notes \lygabc
  }

  \lilygabc-vaticana-lyrics "voiceid" \lygabc
  >>
}
