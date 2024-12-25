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

  ragged-bottom = ##t
}

gabc-score-with-solfege =
#(define-scheme-function (gabc) (string?)
   (let ((ligabc (lilygabc-parse-gabc gabc))
         (voiceid "voiceid"))
     #{
       <<
         \lilygabc-vaticana-voice #voiceid #ligabc

         \new NoteNames {
           \set printNotesLanguage = "italiano"
           \override NoteName.font-size = #-4

           \lilygabc-vaticana-notes #ligabc
         }

         \lilygabc-vaticana-lyrics #voiceid #ligabc
       >>
     #}))

\score {
  % music from https://gregobase.selapa.net/chant.php?id=8653
  \gabc-score-with-solfege
    "(c4)Re(h)rum(h') De(h)us(h) te(h)nax(h') vi(g)gor,(g'_) (,)
    Im(h)mó(h')tus(h) in(h) te(g) pér(h')ma(h)nens,(h.) (;z)
    Lu(h)cis(h') di(h)úr(g)næ(f) tém(g')po(f)ra(e'_) (,)
    Suc(f)cés(g')si(g)bus(g) de(f)tér(h')mi(g)nans.(g.) (::)"
  \layout {
    ragged-last = ##f
  }
}

\pageBreak

\score {
  % music from https://gregobase.selapa.net/chant.php?id=2539
  \gabc-score-with-solfege
    "(f3) Ut(e) que(f')ant(h) la(fg)xis(f.) (,)
    re(f')so(f)ná(e')re(f) fi(g)bris(g.) (;)
    Mi(g!hwi)ra(g) ge(f)stó(ge)rum(f.) (,z)
    fá(h')mu(i)li(j) tu(i)ó(hf)rum,(f.) (:)
    Sol(iji)ve(hg) pol(h)lú(i)ti(f.) (,)
    lá(j')bi(i)i(j) re(hi)á(j)tum,(j.) (,)
    San(ih)cte(gf) Jo(e)án(g)nes.(f.) (::)"
}
