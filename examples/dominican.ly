\version "2.24.1"

% Dominican divisiones

\header {
  title = "Dominican Chant"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 30)

\layout {
  \override VaticanaLyrics.LyricText.font-size = #-2
  \override VaticanaStaff.StaffSymbol.color = #black
}

\paper {
  #(define fonts
     (set-global-fonts
      #:roman "Linux Libertine O"
      #:factor (/ staff-height pt 20)))
}

\score {
  % music from https://gregobase.selapa.net/chant.php?id=7283
  %
  % TODO GregoBase has ,4 instead of ;4 (etc.) and Gregorio swallows that -
  %   is it a Gregorio quirk exploited in the wild, or an official alternative
  %   syntax which we should support?
  \gabc-vaticana
    "(c4) Ma(gvFD)num(fg) (::) su(ghg)am(g) (;4)
    a(f)pé(g)ru(h)it(ji) í(hg)no(f)pi,(g) (;)
    et(h) pal(j)mas(g) su(gh)as(g) ex(g)tén(fe)dit(d) (;1)
    ad(fhg) pau(f)pé(fg)rem:(g) (:)
    for(hj)ti(i)tú(jk)do(h) et(g) de(h)cor(g) (;1)
    in(f)du(h)mén(jh)tum(ih) e(g)jus:(g) (;)
    et(ge) ri(g)dé(fe)bit(f) (;1)
    in(d) di(ef)e(gh) no(h)vís(g)si(g)mo.(g) <i>T.P.</i>(::)
    mo,(gvFD) al(fe~)le(fg)lú(g)ia.(g) <i>Cant.</i>(::)
    Be(g)ne(h)dic(gj)tus(j) (::)
    E(j) u(j) o(i) u(j) a(h) e.(g) (::)"
  \header {
    piece = \markup\tiny{Antiphonarium S. O. P., Romae 1933, p. 108*}
  }
}
