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
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" a, } }
    \score { \gabc-vaticana "(c4) (a)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" b, } }
    \score { \gabc-vaticana "(c4) (b)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" c } }
    \score { \gabc-vaticana "(c4) (c)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" d } }
    \score { \gabc-vaticana "(c4) (d)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" e } }
    \score { \gabc-vaticana "(c4) (e)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" f } }
    \score { \gabc-vaticana "(c4) (f)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" g } }
    \score { \gabc-vaticana "(c4) (g)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" a } }
    \score { \gabc-vaticana "(c4) (h)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" b } }
    \score { \gabc-vaticana "(c4) (i)" }
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" c' } }
    \score { \gabc-vaticana "(c4) (j)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" d' } }
    \score { \gabc-vaticana "(c4) (k)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" e' } }
    \score { \gabc-vaticana "(c4) (l)" }
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" f' } }
    \score { \gabc-vaticana "(c4) (m)" }

    "" "" "" ""
  }
}

\bookpart {
  \header { subtitle = "clef positions" }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g } }
    \score { \gabc-vaticana "(c4) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do2" b } }
    \score { \gabc-vaticana "(c3) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do1" d' } }
    \score { \gabc-vaticana "(c2) (g)" }

    % TODO not included in the automated test suite, because \displayLilyMusic
    % handles custom clefs strangely. Investigate what's going on.
    \score { \new VaticanaVoice { \clef "vaticana-do0" f' } }
    \score { \gabc-vaticana "(c1) (g)" }
  }

  \markup\fill-line{
    % @test clef change
    \score { \new VaticanaVoice { \clef "vaticana-do3" g \clef "vaticana-do2" b } }
    \score { \gabc-vaticana "(c4) (g) (c3) (g)" }

    "" "" "" "" "" ""
  }

  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-fa3" c } }
    \score { \gabc-vaticana "(f4) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-fa2" e } }
    \score { \gabc-vaticana "(f3) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-fa1" g } }
    \score { \gabc-vaticana "(f2) (g)" }

    \score { \new VaticanaVoice { \clef "vaticana-fa0" b } }
    \score { \gabc-vaticana "(f1) (g)" }
  }
}

\bookpart {
  \header { subtitle = "accidentals" }

  \markup { inline: }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" ges } }
    \score { \gabc-vaticana "(c4) (gxg)" }

    % LilyPond - at least in the default gregorian.ly settings -
    % seems to never automatically produce natural signs.
    % We have to request them explicitly.
    \score { \new VaticanaVoice { \clef "vaticana-do3" ges4( g!) } }
    \score { \gabc-vaticana "(c4) (gxggyg)" \layout { \xfail } }
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" gis4 } }
    \score { \gabc-vaticana "(c4) (g#g)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" gis4( g!) } }
    \score { \gabc-vaticana "(c4) (g#ggyg)" \layout { \xfail } }
  }

  % accidentals last until the end of the word
  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" ges4 g } }
    \score { \gabc-vaticana "(c4) (gxg) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" gis4 g } }
    \score { \gabc-vaticana "(c4) (g#g) (g)" }
  }

  \markup { clefs with b flat: }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" { \key f \major } bes4 } }
    \score { \gabc-vaticana "(cb4) (i)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" { \key f \major } b4 } }
    \score { \gabc-vaticana "(cb4) (iyi)" }
  }

  % natural lasts until the end of the word
  \markup\fill-line{
    \score { \new VaticanaVoice { \clef "vaticana-do3" \key f \major b b } \addlyrics { la -- la } }
    \score { \gabc-vaticana "(cb4) la(iyi)la(i)" \layout { \xfail } }

    \score { \new VaticanaVoice { \clef "vaticana-do3" \key f \major b bes } \addlyrics { la la } }
    \score { \gabc-vaticana "(cb4) la(iyi) la(i)" \layout { \xfail } }
  }

  % clef changes and b flat
  \markup\fill-line{
    % On clef change LilyPond - at least in the default gregorian.ly settings -
    % doesn't render the key signature, so we have to insert it "manually"
    % in case of gabc clefs with b flat.
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" { \key f \major } g4 \clef "vaticana-do2" { \key f \major } bes } }
    \score { \gabc-vaticana "(cb4) (g) (cb3) (g)" }


    \score { \new VaticanaVoice {
      \clef "vaticana-do3" \key f \major g4
      \clef "vaticana-do2" \key c \major b
    } }
    \score { \gabc-vaticana "(cb4) (g) (c3) (g)" \layout { \xfail } }
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g4 \clef "vaticana-do2" { \key f \major } bes } }
    \score { \gabc-vaticana "(c4) (g) (cb3) (g)" }

    "" ""
  }
}

\bookpart {
  \header { subtitle = "divisiones" }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g \divisioMinima g } }
    \score { \gabc-vaticana "(c4) (g) (,) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g \divisioMaior g } }
    \score { \gabc-vaticana "(c4) (g) (;) (g)" }
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g \divisioMaxima g } }
    \score { \gabc-vaticana "(c4) (g) (:) (g)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g \finalis g } }
    \score { \gabc-vaticana "(c4) (g) (::) (g)" }
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" g \virgula g } }
    \score { \gabc-vaticana "(c4) (g) (`) (g)" }

    "" ""
  }

  % all the supported exotic forms
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
