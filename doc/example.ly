\version "2.24.1"

\include "../lilygabc.ily"

\header {
  tagline = #f
}

\paper {
  #(define fonts
     (make-pango-font-tree
      "Linux Libertine O"
      "VL Gothic"
      "Courier"
      1))

  top-margin = 1.5\cm
}

\layout {
  \lilygabcModernGregorianStemlessLayout

  indent = 0
  \set Score.forbidBreakBetweenBarLines = ##f
}

\score {
  \gabc
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
    E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"
  \header {
    piece = "Ant. IV E"
  }
}

% https://gregobase.selapa.net/chant.php?id=13305
\score {
  \gabc
    "(c4) Tres(g) pú(g')e(g)ri(gh) jus(hv_GF)su(gh) re(h)gis(g'_) (,) in(f) for(hj~)ná(j)cem(i) mis(jk)si(h) sunt,(g.) (;) non(j) ti(i')mén(j)tes(h') flam(j)mam(g') i(h)gnis,(gf__) (;) di(d)cén(f_g)tes:(g_f) Be(h)ne(ji)dí(hg)ctus(fg) De(g)us,(gf) al(gh)le(h)lú(g.)ia.(g.) (::) E(j) u(j) o(i) u(j) a(h) e.(g) (::)"
  \header {
    piece = "Ant. VIII G"
  }
}