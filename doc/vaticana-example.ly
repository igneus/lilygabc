\version "2.24.1"

\header {
  tagline = #f
}

my-roman-font = "Linux Libertine O"

\paper {
  % (Global font settings have no effect in VaticanaLyrics,
  % this is just a failed attempt and a reminder of a probable LilyPond bug.)
  #(define fonts
     (make-pango-font-tree
      my-roman-font
      "VL Gothic"
      "Courier"
      1))

  top-margin = 1.5\cm
  left-margin = 1\mm
  right-margin = 1\mm
}

\layout {
  % This actually sets lyrics font for VaticanaLyrics
  \override VaticanaLyrics.LyricText.font-name = \my-roman-font
}

\include "gregorian.ly"
\include "../lilygabc.ily"

% https://gregobase.selapa.net/chant.php?id=12115
\score {
  \gabc-vaticana
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
    E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"
  \layout {
    \lilygabcVaticanaLayout
  }
  \header {
    piece = "Ant. IV E"
  }
}
