\version "2.24.1"

\include "../../lilygabc.ily"
\include "helper.ily"

\header {
  title = "lilygabc visual tests - modern notation"
}

\layout {
  \context {
    \Voice
    \consists Episema_engraver
  }
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
  \score { \gabc "(c4) (a)" \layout { \xfail } }
  \markup\justify{
    are known failing tests,
    showcasing features not yet supported by lilygabc or known bugs/limitations.
  }

  \markup\justify{
    Additionally some of the example pairs are included in a fully automated
    test suite comparing LilyPond data structures.
    (Marked in the source code of this document with a comment
    beginning with the \typewriter{@test} annotation.)
    In those test examples the hand-coded expected result specifies not only
    the expected visual outcome, but also the expected internal representation
    of the music.
  }
}

\bookpart {
  \header { subtitle = "single note" }

  \markup\fill-line{
    % @test
    \score { { a4 } }
    \score { \gabc "(c4) (a)" }

    % @test
    \score { { b4 } }
    \score { \gabc "(c4) (b)" }

    % @test
    \score { { c'4 } }
    \score { \gabc "(c4) (c)" }
  }

  \markup\fill-line{
    \score { d'4 }
    \score { \gabc "(c4) (d)" }

    \score { e'4 }
    \score { \gabc "(c4) (e)" }

    \score { f'4 }
    \score { \gabc "(c4) (f)" }
  }

  \markup\fill-line{
    \score { g'4 }
    \score { \gabc "(c4) (g)" }

    \score { a'4 }
    \score { \gabc "(c4) (h)" }

    \score { b'4 }
    \score { \gabc "(c4) (i)" }
  }

  \markup\fill-line{
    \score { c''4 }
    \score { \gabc "(c4) (j)" }

    \score { d''4 }
    \score { \gabc "(c4) (k)" }

    \score { e''4 }
    \score { \gabc "(c4) (l)" }
  }

  \markup\fill-line{
    \score { f''4 }
    \score { \gabc "(c4) (m)" }

    "" "" "" ""
  }
}

\bookpart {
  \header { subtitle = "multiple notes" }

  \markup\fill-line{
    % @test
    \score { { c'4 d' } }
    \score { \gabc "(c4) (c) (d)" }

    % @test
    \score { { e'4 f' g' } }
    \score { \gabc "(c4) (e) (f) (g)" }
  }
}

allGabcNotes = " (a) (b) (c) (d) (e) (f) (g) (h) (i) (j) (k) (l) (m)"

\bookpart {
  \header { subtitle = "clef positions" }

  \markup\justify{
    The left column shows only the expected ambitus,
    right column all gabc notes interpreted under the given clef.
  }

  \markup\line{
    \score { \relative { c'4 a'' } }
    \score { \gabc \allGabcNotes } % clef not specified in gabc
  }

  \markup\line{
    \score { \relative { a4 f'' } }
    \score { \gabc #(string-append "(c4)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { c'4 a'' } }
    \score { \gabc #(string-append "(c3)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { e'4 c'' } }
    \score { \gabc #(string-append "(c2)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { g'4 e'' } }
    \score { \gabc #(string-append "(c1)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { d4 b'' } }
    \score { \gabc #(string-append "(f4)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { f4 d'' } }
    \score { \gabc #(string-append "(f3)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { a4 f'' } }
    \score { \gabc #(string-append "(f2)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { c'4 a'' } }
    \score { \gabc #(string-append "(f1)" allGabcNotes) }
  }
}

\bookpart {
  \header { subtitle = "clef changes" }

  \markup\line{
    \score { \relative { d'4 f a c } }
    \score { \gabc "(c4) (d) (c3) (d) (c2) (d) (c1) (d)" }

    "" ""
  }
}

\bookpart {
  \header { subtitle = "accidentals" }

  \markup { inline: }

  \markup\line{
    % @test
    \score { { ges'4 } }
    \score { \gabc "(c4) (gxg)" }

    % @test
    \score { { ges'4( g') } }
    \score { \gabc "(c4) (gxggyg)" }
  }

  \markup\line{
    % @test
    \score { { gis'4 } }
    \score { \gabc "(c4) (g#g)" }

    % @test
    \score { { gis'4( g') } }
    \score { \gabc "(c4) (g#ggyg)" }
  }

  % accidentals last until the end of the word
  \markup\line{
    % @test
    \score { { ges'4 g' } }
    \score { \gabc "(c4) (gxg) (g)" }

    % @test
    \score { { gis'4 g' } }
    \score { \gabc "(c4) (g#g) (g)" }
  }

  \markup { clefs with b flat: }

  \markup\line{
    % @test
    \score { { \key f \major bes'4 } }
    \score { \gabc "(cb4) (i)" }

    % @test
    \score { { \key f \major b'4 } }
    \score { \gabc "(cb4) (iyi)" }
  }

  % natural lasts until the end of the word
  \markup\line{
    % @test
    \score { { \key f \major b' b' } \addlyrics { la -- la } }
    \score { \gabc "(cb4) la(iyi)la(i)" }

    % @test
    \score { { \key f \major b' bes' } \addlyrics { la la } }
    \score { \gabc "(cb4) la(iyi) la(i)" }
  }

  % clef changes and b flat
  \markup\line{
    % @test
    \score { { \key f \major g'4 bes' } }
    \score { \gabc "(cb4) (g) (cb3) (g)" }

    % @test
    \score { { \key f \major g'4 \key c \major b' } }
    \score { \gabc "(cb4) (g) (c3) (g)" }
  }

  \markup\line{
    % @test
    \score { { g'4 \key f \major bes' } }
    \score { \gabc "(c4) (g) (cb3) (g)" }

    "" ""
  }

  \markup{musica ficta:}

  \markup\line{
    % @test musica ficta flat
    \score { { { \once \set suggestAccidentals = ##t bes'4 } } }
    \score { \gabc "(c3) (gr6)" }

    % @test musica ficta natural
    \score { { \key f \major { \once \set suggestAccidentals = ##t b'4 } } }
    \score { \gabc "(cb3) (gr7)" }
  }

  \markup\line{
    % @test musica ficta sharp
    \score { { { \once \set suggestAccidentals = ##t gis'4 } } }
    \score { \gabc "(c4) (gr8)" }

    "" ""
  }
}

\bookpart {
  \header { subtitle = "melismata" }

  \markup\fill-line{
    % @test
    \score { { g'4( a') } }
    \score { \gabc "(c4) (gh)" }

    % @test
    \score { { g'4( a' g') } }
    \score { \gabc "(c4) (ghg)" }
  }

  \markup\fill-line{
    % @test
    \score { { g'4 g'( a') g' } }
    \score { \gabc "(c4) (g) (gh) (g)" }

    ""
  }

  \markup\fill-line{
    % @test
    \score { { g'4( a' c'' a' b' a' g') } }
    \score { \gabc "(c4) (ghjhihg)" }

    ""
  }
}

\bookpart {
  \header { subtitle = "note shapes, articulations, other music elements" }

  % articulations (limited support), note shapes, adiastematic neumes (ignored)

  \markup\fill-line{
    % @test nabc
    \score { { g'4 } }
    \score { \gabc "(c4) (g|vihg)" }

    % @test punctum inclinatum
    \score { { g'4( { \once \tiny f' } { \once \tiny e' } { \once \tiny d') } g' } }
    \score { \gabc "(c4) (gFED) (g)" }
  }

  \markup\fill-line{
    % @test diminutive liquescence
    \score { { g'4( { \once \teeny f') } } }
    \score { \gabc "(c4) (gf~)" }

    "" ""
  }

  \markup\fill-line{
    % @test punctum mora
    \score { { g'4. g'4 } }
    \score { \gabc "(c4) (g.) (g)" }

    % @test double punctum mora
    \score { { g'4.. g'4 } }
    \score { \gabc "(c4) (g..) (g)" }
  }

  \markup\fill-line{
    % @test ictus / vertical episema
    \score { { g'4_! } }
    \score { \gabc "(c4) (g')" }

    % @test horizontal episema
    \score { { g'4\episemInitium\episemFinis } }
    \score { \gabc "(c4) (g_)" }
  }

  \markup\fill-line{
    % @test all supported articulations at once
    \score { { g'4.\episemInitium\episemFinis_! } }
    \score { \gabc "(c4) (g._')" }

    % @test articulations on a punctum inclinatum
    \score { { g'4( { \once \tiny f')\episemInitium\episemFinis_! } } }
    \score { \gabc "(c4) (gF_')" }
  }

  \markup\fill-line{
    % @test horizontal episema spanning multiple notes
    \score { { g'4(\episemInitium f')\episemFinis } }
    \score { \gabc "(c4) (g_f_)" }

    "" ""
  }

  \markup\fill-line{
    \score {
      \relative {
        \stemDown
        \once \revert Stem.length
        \once \override NoteHead.stem-attachment = #'(0.8 . 0.3)
        g'4( \tiny f e d) \normalsize g
      }
      \layout { \lilygabcModernGregorianStemlessLayout } % virga only makes a difference in the stemless settings
    }
    \score {
      \gabc "(c4) (gvFED) (g)" % virga right
      \layout { \lilygabcModernGregorianStemlessLayout }
    }

    \score {
      \relative {
        \stemDown
        \once \revert Stem.length
        g'4( \tiny f e d) \normalsize g
      }
      \layout { \lilygabcModernGregorianStemlessLayout }
    }
    \score {
      \gabc "(c4) (gVFED) (g)" % virga left
      \layout { \lilygabcModernGregorianStemlessLayout }
    }
  }

  \markup\fill-line{
    % when stems are not hidden by default, virga should have no visible effect:
    \score { \relative { g'4 g( \tiny f e d) \normalsize } }
    \score { \gabc "(c4) (g) (gvFED)" \layout { \xfail } } % virga right

    \score { \relative { g'4 g( \tiny f e d) \normalsize } }
    \score { \gabc "(c4) (g) (gVFED)" \layout { \xfail } } % virga left
  }

  \markup\fill-line{
    % @test torculus initio debilis
    \score { { { \once \teeny g'4( } a' g') } }
    \score { \gabc "(c4) (-ghg)" }

    "" ""
  }

  \markup\fill-line{
    % quilisma with a special notehead from the Vaticana set - neither beautiful, nor easily recognizable
    \score { { g'4( \once \override NoteHead.style = #'vaticana.quilisma \once \override NoteHead.font-size = #7 a' b' ) } \layout { \wontfix } }
    \score { \gabc "(c4) (ghwi)" }

    % abusing a standard articulation sign like in the Czech LOTH hymnal
    % @test quilisma
    \score { { g'4( a'\prall b' ) } }
    \score { \gabc "(c4) (ghwi)" }
  }

  \markup\fill-line{
    % bivirga
    \score { { g'4( g') } }
    \score { \gabc "(c4) (gvv)" \layout { \xfail } }

    % trivirga
    \score { { g'4( g' g') } }
    \score { \gabc "(c4) (gvvv)" \layout { \xfail } }
  }

  \markup\fill-line{
    % @test bistropha
    \score { { g'4( g') } }
    \score { \gabc "(c4) (gss)" }

    % @test tristropha
    \score { { g'4( g' g') } }
    \score { \gabc "(c4) (gsss)" }
  }
}

\bookpart {
  \header { subtitle = "divisiones" }

  \markup\fill-line{
    % @test
    \score { { g'4 \bar "'" g' } }
    \score { \gabc "(c4) (g) (,) (g)" }

    % @test
    \score { { g'4 \bar "," g' } }
    \score { \gabc "(c4) (g) (;) (g)" }
  }

  \markup\fill-line{
    % @test
    \score { { g'4 \bar "|" g' } }
    \score { \gabc "(c4) (g) (:) (g)" }

    % @test
    \score { { g'4 \bar "||" g' } }
    \score { \gabc "(c4) (g) (::) (g)" }
  }

  \markup\fill-line{
    % @test
    \score { { g'4 \breathe g' } }
    \score { \gabc "(c4) (g) (`) (g)" }

    "" ""
  }

  % all the supported exotic forms
}

\bookpart {
  \header { subtitle = "line breaks" }

  % @test line break
  \score { { g'4 \break g' } }
  \score { \gabc "(c4) (g) (z) (g)" }
}

\bookpart {
  \header { subtitle = "lyrics alignment" }

  \markup\fill-line{
    % @test
    \score { { g'4 } \addlyrics { La } }
    \score { \gabc "(c4) La(g)" }

    % @test
    \score { { g'4 g' } \addlyrics { La la } }
    \score { \gabc "(c4) La(g) la(g)" }
  }

  \markup\fill-line{
    % @test
    \score { { g'4 g' } \addlyrics { La -- la } }
    \score { \gabc "(c4) La(g)la(g)" }

    "" ""
  }

  \markup\fill-line{
    % @test
    \score { { g'4( a' g') f'( g') } \addlyrics { A -- men. } }
    \score { \gabc "(c4) A(ghg)men.(fg)" }

    ""
  }

  \markup\fill-line{
    % @test : lyrics with no music
    \score { { g'4 { \once \hideNotes g' } g' } \addlyrics { La "*" la } }
    \score { \gabc "(c4) La(g) *() la(g)" }

    % @test : music with no lyrics
    \score { { g'4 g' g' } \addlyrics { La "" la } }
    \score { \gabc "(c4) La(g) (g) la(g)" }
  }

  \markup\fill-line{
    % @test : void syllable
    \score { { g'4 \bar "" g' } \addlyrics { La la } }
    \score { \gabc "(c4) La(g) () la(g)" }

    "" ""
  }

  \markup\fill-line{
    % lyrics under a divisio:
    % LilyPond doesn't support that -> use an invisible note as a workaround
    % @test
    \score { { g'4 { \once \hideNotes g' } \bar "'" g' } \addlyrics { La "*" la } }
    \score { \gabc "(c4) La(g) *(,) la(g)" }

    % @test
    \score { { g'4 { \once \hideNotes g' } \bar "|" g' } \addlyrics { La T.P. la } }
    \score { \gabc "(c4) La(g) T.P.(:) la(g)" }
  }

  \markup\fill-line{
    % @test : divisio inside a music syllable
    \score { { g'4( a' g' \bar "'" g' c'') c'' } \addlyrics { La la } }
    \score { \gabc "(c4) La(ghg,gj) la(j)" }

    ""
  }

  \markup\fill-line{
    % @test formatting tags (for the time being ignored)
    \score { { g'4 } \addlyrics { La } }
    \score { \gabc "(c4) <b>La</b>(g)" }

  }
}

\bookpart {
  \header { subtitle = "lyrics special characters" }

  \markup\fill-line{
    % @test response
    \score { { g'4 } \addlyrics { ℟ } }
    \score { \gabc "(c4) <sp>R/</sp>(g)" }

    % @test versicle
    \score { { g'4 } \addlyrics { ℣ } }
    \score { \gabc "(c4) <sp>V/</sp>(g)" }
  }

  \markup\fill-line{
    % barred A
    \score { { g'4 } \addlyrics { A/ } }
    \score { \gabc "(c4) <sp>A/</sp>(g)" \layout { \xfail } }

    "" ""
  }
}

\bookpart {
  \header { subtitle = "test cases from encountered bugs" }

  % TODO add support for multi-line examples, include this one in the automated test suite
  \score {
    \relative { a'4( bes) g g^-_! \bar "'" g g g a g g g( f) g g4.( a) }
    \addlyrics { O -- mni -- a in sa -- pi -- én -- ti -- a fe -- cís -- ti. }
  }
  \score { \gabc "(c4) O(ixhi)mni(g)a(g_') (,) in(g) sa(g)pi(g)én(h)ti(g)a(g) fe(gf)cís(g)ti.(g.h.)" }
}

\bookpart {
  \header {
    subtitle = "remixing gabc-sourced music"
    subsubtitle = "integrating in - and interacting with - other LilyPond constructs"
  }

  \score {
    \transpose d fis {
      \key c \major
      \gabc "(c4) A(ghg)men.(fg) (::)"
    }
    \layout { \lilygabcModernGregorianLayout }
    \header {
      piece = "transposition"
    }
  }

  \score {
    <<
      \new Voice = "v" { \gabc "(c4) (ghg) (fg) (::)" }
      \new Lyrics \lyricsto "v" { A -- men. }
      \new Lyrics \lyricsto "v" { La -- la. }
    >>
    \layout { \lilygabcModernGregorianLayout }
    \header {
      piece = "gabc music + LilyPond lyrics"
    }
  }

  \score {
    {
      \relative c'' { c4 c c c c c c c c }

      \gabc "(c4) A(ij)men.(j) (;)"

      \gabc "(c4) Al(j)le(k)lu(ji)ia.(i) (::)"
    }
    \addlyrics { et in sae -- cu -- la sae -- cu -- lo -- rum. }
    \layout { \lilygabcModernGregorianLayout }
    \header {
      piece = "multiple gabc pieces in a single LilyPond score"
    }
  }

  % TODO: parallel music
}

\bookpart {
  \header { subtitle = "real-life score" }

  \markup\justify{
    (There is no hand-coded expected result,
    all scores showcase lilygabc chant rendering using the provided styles.)
  }
  \score {
    \gabc
      "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
      o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
      E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"
    \header {
      piece = \markup\with-url "https://gregobase.selapa.net/chant.php?id=12115" {GregoBase 12115}
    }
    % LilyPond defaults (bars, time signature):
    \layout {}
    % quarter notes with stems:
    \layout {
      \lilygabcModernGregorianLayout
    }
    % stemless quarter notes:
    \layout {
      \lilygabcModernGregorianStemlessLayout
    }
  }

  \score {
    \gabc-file "../examples/aquam_quam_ego.gabc"
    \layout {
      \lilygabcModernGregorianStemlessLayout
    }
    \header {
      piece = "score loaded from a file"
    }
  }

  \score {
    \gabc "(c4) BE(f|pu)ne(g|vi)dí(gh|vihg)xit(f|pu) *()
    fí(g|vi)li(fe|cl)is(d|pu) tu(f|vi)is(g|vi) in(g|vi>) te.(f|pu) (::)"
    \layout {
      \lilygabcModernGregorianStemlessLayout
    }
    \header {
      piece = "score with nabc (nabc is ignored)"
    }
  }
}
