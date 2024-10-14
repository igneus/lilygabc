\version "2.24.1"

\include "../../lilygabc.ily"
\include "helper.ily"
\include "gregorian.ly"

\header {
  title = "lilygabc visual tests - square notation"
}

\bookpart {
  \header { subtitle = "Introduction" }

  \markup\justify{
    Unless stated otherwise, each section consists
    of lines, each containing one or more \bold{score pairs.}
    In each pair the first score is a manually coded expected result,
    the second one is actual result produced by lilygabc.
    For a test to be considered successful the two scores must be the same.
  }

  \markup\justify{
    Scores marked with a background colour
  }
  \score {
    \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma g g\melismaEnd \] }
    \layout { \xfail }
  }
  \markup\justify{
    are known failing tests,
    showcasing features not yet supported by lilygabc or known bugs/limitations.
  }
}

\bookpart {
  \header { subtitle = "single note" }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" a, } }
    \score { \gabc-vaticana "(c4) (a)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" b, } }
    \score { \gabc-vaticana "(c4) (b)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" c } }
    \score { \gabc-vaticana "(c4) (c)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" d } }
    \score { \gabc-vaticana "(c4) (d)" }

    \score { \displayMusic \new VaticanaVoice { \clef "vaticana-do3" e } }
    \score { \gabc-vaticana "(c4) (e)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" f } }
    \score { \gabc-vaticana "(c4) (f)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" g } }
    \score { \gabc-vaticana "(c4) (g)" }

    \score { \displayMusic \new VaticanaVoice { \clef "vaticana-do3" a } }
    \score { \gabc-vaticana "(c4) (h)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" b } }
    \score { \gabc-vaticana "(c4) (i)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" c' } }
    \score { \gabc-vaticana "(c4) (j)" }

    \score { \displayMusic \new VaticanaVoice { \clef "vaticana-do3" d' } }
    \score { \gabc-vaticana "(c4) (k)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" e' } }
    \score { \gabc-vaticana "(c4) (l)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" f' } }
    \score { \gabc-vaticana "(c4) (m)" }

    "" "" "" ""
  }
}

\bookpart {
  \header { subtitle = "real-life score" }

  \markup\justify{
    (There is no hand-coded expected result,
    all scores showcase lilygabc chant rendering using the provided styles.)
  }
  \score {
    \gabc-vaticana
      "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
      o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
      E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"
    \header {
      piece = \markup\with-url "https://gregobase.selapa.net/chant.php?id=12115" {GregoBase 12115}
    }
  }

  \score {
    \gabc-vaticana-file "../examples/aquam_quam_ego.gabc"
    \header {
      piece = "score loaded from a file"
    }
  }

  \score {
    \gabc-vaticana "(c4) BE(f|pu)ne(g|vi)dí(gh|vihg)xit(f|pu) *()
    fí(g|vi)li(fe|cl)is(d|pu) tu(f|vi)is(g|vi) in(g|vi>) te.(f|pu) (::)"
    \header {
      piece = "score with nabc (nabc is ignored)"
    }
  }
}
