\version "2.24.1"

\header {
  title = "Hymns"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 32)

\layout {
  \override VaticanaStaff.StaffSymbol.color = "black"

  \override VaticanaLyrics.LyricText.font-size = #-2
  \override VaticanaLyrics.StanzaNumber.font-size = #-2
}

\paper {
  #(set-paper-size "a5")

  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 25)))

  scoreTitleMarkup =
    \markup\fill-line { "" \tiny\smallCaps\fromproperty #'header:piece "" }

  markup-markup-spacing.basic-distance = #4
  markup-system-spacing.basic-distance = #1
}

\score {
  <<
    % music from https://gregobase.selapa.net/chant.php?id=8653
    \gabc-vaticana
      "(c4)Re(h)rum(h') De(h)us(h) te(h)nax(h') vi(g)gor,(g'_) (,)
      Im(h)mó(h')tus(h) in(h) te(g) pér(h')ma(h)nens,(h.) (;)
      Lu(h)cis(h') di(h)úr(g)næ(f) tém(g')po(f)ra(e'_) (,)
      Suc(f)cés(g')si(g)bus(g) de(f)tér(h')mi(g)nans.(g.) (::)"

    \new VaticanaLyrics \lyricsto "uniqueContext0" { % TODO voice ID relies on lilygabc internals
      \set stanza = "2."
      Lar -- gí -- re lu -- men vé -- spe -- re,
      Quo vi -- ta nu -- squam dé -- ci -- dat,
      Sed prǽ -- mi -- um mor -- tis sa -- cræ
      Pe -- rén -- nis in -- stet gló -- ri -- a.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" {
      \set stanza = "3."
      Præ -- sta, Pa -- ter pi -- ís -- si -- me,
      Pa -- trí -- que com -- par Ú -- ni -- ce,
      Cum Spí -- ri -- tu Pa -- rá -- cli -- to
      Re -- gnans per om -- ne sǽ -- cu -- lum.
    }
  >>
  \header {
    piece = "Rerum Deus tenax vigor"
  }
}

\score {
  <<
    % music based on https://gregobase.selapa.net/chant.php?id=12999
    \gabc-vaticana
      "(c4) Ve(f)xil(gh)la(ixi) Re(hvGF')gis(g) pró(ghg)de(f)unt:(e.d.) (;)
      Ful(g)get(g') Cru(h)cis(fd) my(f)sté(ef)ri(d)um,(c.d.) (:)
      Qua(d) vi(d')ta(f) mor(dc)tem(f) pér(fgh)tu(g)lit,(g.f.) (;)
      Et(f) mor(ixfh!ivHG')te(h) vi(fd)tam(f) pró(ef)tu(d)lit.(c.d.) (::)"

    \new VaticanaLyrics \lyricsto "uniqueContext0" { % TODO voice ID relies on lilygabc internals
      \set stanza = "2."
      Quæ, vul -- ne -- rá -- ta lán -- ce -- æ
      Mu -- cró -- ne di -- ro, crí -- mi -- num
      Ut nos la -- vá -- ret sór -- di -- bus,
      Ma -- ná -- vit \markup\concat{und \italic{a}} et sán -- gui -- ne.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" {
      \set stanza = "3."
      Im -- plé -- ta sunt quæ cón -- ci -- nit
      Da -- vid fi -- dé -- li cár -- mi -- ne,
      Di -- cén -- do na -- ti -- ó -- ni -- bus:
      Re -- gná -- vit a li -- gno De -- us.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" {
      \set stanza = "4."
      Ar -- bor de -- \markup\concat{cór \italic{a}} et fúl -- gi -- da,
      Or -- ná -- ta Ré -- gis púr -- pu -- ra,
      E -- léc -- ta di -- gno stí -- pi -- te
      Tam sanc -- ta mem -- bra tán -- ge -- re.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" {
      \set stanza = "5."
      Be -- á -- ta, cu -- ius brác -- chi -- is
      Pré -- tium pe -- pén -- dit sǽ -- cu -- li,
      Sta -- té -- ra fac -- ta cór -- po -- ris,
      Tu -- lít -- que præ -- dam tár -- ta -- ri.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" {
      \set stanza = "6."
      O Crux, a -- ve, spes ú -- ni -- ca,
      In hac tri -- úm -- phi gló -- ri -- a
      Pi -- is ad -- áu -- ge grá -- ti -- am,
      Re -- ís -- que de -- le crí -- mi -- na.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" {
      \set stanza = "7."
      Te, fons sa -- lú -- tis, Trí -- ni -- tas,
      Col -- láu -- det om -- nis spí -- ri -- tus:
      Qui -- bus Cru -- cis vic -- tó -- ri -- am
      Lar -- gí -- ris, ad -- de prǽ -- mi -- um.
    }
  >>
  \header {
    piece = "Vexilla Regis"
  }
}
