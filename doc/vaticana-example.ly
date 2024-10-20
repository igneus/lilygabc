\version "2.24.1"

\header {
  tagline = #f
}

\paper {
  % TODO: font settings have no effect in VaticanaLyrics and I've no idea why
  #(define fonts
     (make-pango-font-tree
      "Linux Libertine O"
      "VL Gothic"
      "Courier"
      1))

  top-margin = 1.5\cm
  left-margin = 1\mm
  right-margin = 1\mm
}

\include "gregorian.ly"
\include "../lilygabc.ily"

% https://gregobase.selapa.net/chant.php?id=13305
\score {
  \gabc-vaticana
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
    E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"
  \layout {
    \lilygabcVaticanaLayout
  }
  \header {
    piece = "Ant. VIII G"
  }
}
