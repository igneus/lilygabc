\version "2.24.1"

\header {
  title = "Missal chants"
  subtitle = "Praefatio ferialis communis"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 40)

\layout {
  \override VaticanaLyrics.LyricText.font-size = #0
  \override VaticanaLyrics.StanzaNumber.font-size = #-2
  \override VaticanaLyrics.VerticalAxisGroup.nonstaff-relatedstaff-spacing.basic-distance = #5
}

\paper {
  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 20)))

  scoreTitleMarkup =
    \markup\fill-line { "" \tiny\smallCaps\fromproperty #'header:piece "" }

  markup-system-spacing.basic-distance = #4
}

% Based on https://gregobase.selapa.net/chant.php?id=8030
\score {
  \gabc-vaticana
    "(c3) Per(c) óm(e)ni(f)a(f) sǽ(g)cu(f)la(f) sæ(e)cu(f)ló(fe)rum.(e) (::)
    <sp>R/</sp>.() A(e)men.(ef) (::)
    <sp>V/</sp>.() Dó(e)mi(f)nus(g) vo(ef)bí(g)scum.(f) (::)
    <sp>R/</sp>.() Et(e) cum(f) spí(g)ri(e)tu(f) tu(g)o.(f) (::)
    <sp>V/</sp>.() Sur(h)sum(g) cor(fg)da.(fe) (::)
    <sp>R/</sp>.() Ha(g)bé(h)mus(g) ad(f) Dó(g)mi(f)num.(fe) (::)
    <sp>V/</sp>.() Grá(h)ti(g)as(g) a(f)gá(g)mus(g) (,) Dó(g)mi(g)no,(g) De(e)o(f) no(g)stro.(f) (::)
    <sp>R/</sp>.() Di(h)gnum(g) et(f) ju(g)stum(f) est.(fe) (::)
    (z)

    Ve(f)re(h) di(h)gnum(h) et(h) ju(h)stum(h) est,(h) (,) æ(h)quum(h) et(h) sa(g)lu(f)tá(g)re,(g) (;)
    nos(f) ti(h)bi(h) sem(h)per(h) et(h) u(h)bí(h)que(h) (,) grá(h)ti(g)as(f) á(g)ge(g)re:(g) (;)
    Dó(f)mi(h)ne,(h) san(h)cte(h) Pa(h)ter,(h) om(h)ní(h)po(h)tens(h) æ(h)tér(g)ne(f) De(g)us:(g) (;)
    per(g) Chri(g)stum,(g) Dó(g)mi(e)num(f) no(g)strum.(f) (:)

    Per(f) quem(h) ma(h)je(h)stá(h)tem(h) tu(h)am(h) lau(g)dant(f) Án(g)ge(g)li,(g) (;)
    a(f)dó(h)rant(h) Do(h)mi(h)na(g)ti(f)ó(g)nes,(g) (;) tre(g)munt(g) Po(e)te(f)stá(g)tes.(f) (:)

    Cæ(f)li(h) cæ(h)lo(h)rúm(h)que(h) Vir(h)tú(h)tes(h) (,) ac(h) be(h)á(g)ta(f) Sé(g)ra(g)phim(g) (;)
    só(h)ci(g)a(g) ex(g)sul(g)ta(g)ti(g)ó(g)ne(e) con(f)cé(g)le(f)brant.(f) (:)

    Cum(f) qui(h)bus(h) et(h) nó(h)stras(h) vo(h)ces(h) (,) ut(h) ad(h)mít(h)ti(h) jú(h)be(h)as,(h) de(g)pre(f)cá(g)mur,(g) (;)
    súp(h)pli(g)ci(g) con(g)fes(g)si(g)ó(g)ne(e) di(f)cén(g)tes:(f) (::)"
}
