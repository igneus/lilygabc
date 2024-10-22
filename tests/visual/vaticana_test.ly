\version "2.24.1"

\include "gregorian.ly" % it's important to include gregorian.ly BEFORE lilygabc.ily

\include "../../lilygabc.ily"
\include "helper.ily"

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
  \header { subtitle = "note shapes" }

  % the note shape commands like \virga etc. don't seem to work outside of a melisma
  \markup\fill-line{
    % @test punctum inclinatum
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \inclinatum g \] } }
    \score { \gabc-vaticana "(c4) (G)" }

    % @test diminutive liquescence
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma \flexa \deminutum f\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (gf~)" }
  }

  \markup\fill-line{
    % @test virga right
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \virga g \] } }
    \score { \gabc-vaticana "(c4) (gv)" }

    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \virga g \] } \layout { \xfail } }
    \score { \gabc-vaticana "(c4) (gV)" \layout { \xfail } } % virga left - seems to be unsupported by gregorian.ly, but should be doable with some sort of grob transformation
  }

  \markup\fill-line{
    % @test torculus initio debilis
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \deminutum g\melisma \pes a \flexa g\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (-ghg)" }

    "" ""
  }

  \markup\fill-line{
    % @test punctum ascendens
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \auctum \ascendens g \] } }
    \score { \gabc-vaticana "(c4) (g<)" }

    % @test punctum descendens
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \auctum \descendens g \] } }
    \score { \gabc-vaticana "(c4) (g>)" }
  }

  \markup\fill-line{
    % @test oriscus
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \oriscus g \] } }
    \score { \gabc-vaticana "(c4) (go)" }

    % @test quilisma
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \quilisma g \] } }
    \score { \gabc-vaticana "(c4) (gw)" }
  }
}

\bookpart {
  \header { subtitle = "articulations" }

  \markup\fill-line{
    % @test punctum mora
    \score { \new VaticanaVoice { \clef "vaticana-do3" \augmentum g g } }
    \score { \gabc-vaticana "(c4) (g.) (g)" }

    "" ""
  }

  \markup\fill-line{
    % @test ictus / vertical episema
    \score { \new VaticanaVoice { \clef "vaticana-do3" g\ictus } }
    \score { \gabc-vaticana "(c4) (g')" }

    % @test horizontal episema
    \score { \new VaticanaVoice { \clef "vaticana-do3" g\episemInitium\episemFinis } }
    \score { \gabc-vaticana "(c4) (g_)" } % horizontal episema
  }

  \markup\fill-line{
    % @test all supported articulations at once
    \score { \new VaticanaVoice { \clef "vaticana-do3" \augmentum g\ictus \episemInitium \episemFinis } }
    \score { \gabc-vaticana "(c4) (g._')" }

    % @test articulations on a punctum inclinatum
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ \inclinatum g\ictus \episemInitium \episemFinis \] } }
    \score { \gabc-vaticana "(c4) (G_')" }
  }
}

\bookpart {
  \header { subtitle = "melismata" }

  \markup{
    two notes
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma g\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (gg)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma \pes a\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (gh)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ a\melisma \flexa g\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (hg)" }
  }

  \markup{
    three notes
  }

  \markup\fill-line{
    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma g g\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (ggg)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma \pes a \flexa g\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (ghg)" }

    % @test
    \score { \new VaticanaVoice { \clef "vaticana-do3" \[ g\melisma \flexa f \pes g\melismaEnd \] } }
    \score { \gabc-vaticana "(c4) (gfg)" }
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
  \header { subtitle = "lyrics" }

  \markup\fill-line{
    % @test
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La } >> }
    \score { \gabc-vaticana "(c4) La(g)" }

    % @test
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La la } >> }
    \score { \gabc-vaticana "(c4) La(g) la(g)" }
  }

  \markup\fill-line{
    % @test
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La -- la } >> }
    \score { \gabc-vaticana "(c4) La(g)la(g)" }

    "" ""
  }

  \markup\fill-line{
    % @test
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" \[ g\melisma \pes a \flexa g\melismaEnd \] \[ f\melisma \pes g\melismaEnd \] } \new VaticanaLyrics \lyricsto "uniqueContext0" { A -- men. } >> }
    \score { \gabc-vaticana "(c4) A(ghg)men.(fg)" }

    "" ""
  }

  \markup\fill-line{
    % @test lyrics with no music
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g { \once \hideNotes g' } g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La "*" la } >> }
    \score { \gabc-vaticana "(c4) La(g) *() la(g)" }

    % @test music with no lyrics
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g4 g g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La "" la } >> }
    \score { \gabc-vaticana "(c4) La(g) (g) la(g)" }
  }

  \markup\fill-line{
    % @test void syllable
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g4 \bar "" g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La la } >> }
    \score { \gabc-vaticana "(c4) La(g) () la(g)" }

    "" ""
  }

  \markup\fill-line{
    % lyrics under a divisio:
    % LilyPond doesn't support that -> use an invisible note as a workaround
    % @test
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g4 { \once \hideNotes g' } \divisioMinima g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La "*" la } >> }
    \score { \gabc-vaticana "(c4) La(g) *(,) la(g)" }

    % @test
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" g4 { \once \hideNotes g' } \divisioMaxima g } \new VaticanaLyrics \lyricsto "uniqueContext0" { La T.P. la } >> }
    \score { \gabc-vaticana "(c4) La(g) T.P.(:) la(g)" }
  }

  \markup\justify{
    Probable LilyPond bug:
    in the source code the divisio minima is after the torculus,
    but LilyPond renders it after the pes.
  }
  \markup\fill-line{
    % @test divisio inside a music syllable
    \score { << \new VaticanaVoice = "uniqueContext0" { \clef "vaticana-do3" \[ g4\melisma \pes a \flexa g \divisioMinima g \pes c'\melismaEnd \] c' } \new VaticanaLyrics \lyricsto "uniqueContext0" { La la } >> }
    \score { \gabc-vaticana "(c4) La(ghg,gj) la(j)" }

    "" ""
  }
}

size-example-score = "(c4) A(ghg)men.(fg)"
\bookpart {
  \header { subtitle = "staff size" }

  \markup\justify{
    When living side by side in the same document,
    the modern notation and square notation scores should both look good
    in terms of default element sizes and proportions.
  }

  \markup\fill-line {
    \score {
      \gabc \size-example-score
      \layout { \lilygabcModernGregorianStemlessLayout }
    }
    \score {
      \gabc-vaticana \size-example-score
      \layout { \lilygabcVaticanaLayout }
    }

    "" ""
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
    \layout { \lilygabcVaticanaLayout }
    \header {
      piece = \markup\with-url "https://gregobase.selapa.net/chant.php?id=12115" {GregoBase 12115}
    }
  }

  \score {
    \gabc-vaticana-file "../examples/aquam_quam_ego.gabc"
    \layout { \lilygabcVaticanaLayout }
    \header {
      piece = "score loaded from a file"
    }
  }

  \score {
    \gabc-vaticana "(c4) BE(f|pu)ne(g|vi)dí(gh|vihg)xit(f|pu) *()
    fí(g|vi)li(fe|cl)is(d|pu) tu(f|vi)is(g|vi) in(g|vi>) te.(f|pu) (::)"
    \layout { \lilygabcVaticanaLayout }
    \header {
      piece = "score with nabc (nabc is ignored)"
    }
  }
}
