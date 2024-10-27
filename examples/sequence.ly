\version "2.24.1"

\header {
  title = "Sequence Veni Sancte Spiritus"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 32)

\layout {
  \override VaticanaStaff.StaffSymbol.color = "black"
  \override VaticanaLyrics.LyricText.font-size = #-2
  \override VaticanaLyrics.VerticalAxisGroup.nonstaff-nonstaff-spacing.minimum-distance = #2
  \override VaticanaLyrics.VerticalAxisGroup.nonstaff-relatedstaff-spacing.basic-distance = #4
}

\paper {
  #(set-paper-size "a5")

  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 25)))
}

\score {
  <<
    % based on https://gregobase.selapa.net/chant.php?id=7360
    % odd verses are in the gabc notation, even ones in the lyrics below
    \gabc-vaticana "(c4) Ve(c)ni(d) San(e)cte(f) Spí(ed)ri(c)tus,(d.) (;)
      Et(f) e(g)mít(h)te(ixi) caé(hvGF')li(g)tus(h.) (;)
      Lu(c)cis(d) tu(f)ae(g) rá(fvED')di(c)um.(d.) (::)

      Con(h)so(j)lá(k)tor(k) óp(ji)ti(j)me,(k.) (;)
      Dul(j)cis(h) hos(ji)pes(g) á(fe)ni(d)mae,(c.) (;)
      Dul(g)ce(f) re(gh)fri(g)gé(fvED')ri(c)um.(d.) (::)

      O(k) lux(k) be(ji)a(j)tís(kj)si(i)ma,(h.) (;)
      Re(f)ple(d) cor(c)dis(d) ín(f)ti(g)ma(f.) (;)
      Tu(gh)ó(ixi)rum(h) fi(g)dé(fvED')li(c)um.(d.) (::)

      La(h)va(j) quod(ih) est(i) sór(ji)di(h)dum,(g.) (;)
      Ri(h)ga(h) quod(fe) est(f) á(gf)ri(e)dum,(d.) (;)
      Sa(e)na(g) quod(h) est(g) sáu(j)ci(i)um.(h.) (::)

      Da(k) tu(k)is(g) fi(h)dé(j)li(i)bus,(h.) (;)
      In(h) te(ixi) con(hg)fi(h)dén(f)ti(g)bus,(f.) (;)
      Sac(e)rum(g) sep(h)te(d)ná(f)ri(e)um.(d.) (::)"

    \new VaticanaLyrics \lyricsto "uniqueContext0" {  % TODO voice ID relies on lilygabc internals
      Ve -- ni pa -- ter páu -- pe -- rum,
      Ve -- ni da -- tor mú -- ne -- rum,
      Ve -- ni lu -- men cór -- di -- um.

      In la -- bó -- re ré -- qui -- es,
      In aes -- tu tem -- pé -- ri -- es,
      In fle -- tu so -- lá -- ti -- um.

      Si -- ne tu -- o nú -- mi -- ne,
      Ni -- hil est in hó -- mi -- ne,
      Ni -- hil est in -- nó -- xi -- um.

      Flec -- te quod est rí -- gi -- dum,
      Fo -- ve quod est frí -- gi -- dum,
      Re -- ge quod est dé -- vi -- um.

      Da vir -- tú -- tis mé -- ri -- tum,
      Da sa -- lú -- tis éx -- i -- tum,
      Da per -- én -- ne gáu -- di -- um.
    }
  >>
}
