\version "2.24.1"

\include "../../lilygabc.ily"

\header {
  title = "lilygabc visual tests"
}

\bookpart {
  \header { subtitle = "single note" }

  \markup\justify{
    Each line consists of a series of score pairs.
    The odd scores are manually coded expected results,
    the even ones are actual results produced by lilygabc.
  }

  \markup\fill-line{
    ""

    \score { a4 }
    \score { \gabc "(c4) (a)" }

    \score { b4 }
    \score { \gabc "(c4) (b)" }
  }

  \markup\fill-line{
    \score { c'4 }
    \score { \gabc "(c4) (c)" }

    \score { d'4 }
    \score { \gabc "(c4) (d)" }

    \score { e'4 }
    \score { \gabc "(c4) (e)" }
  }

  \markup\fill-line{
    \score { f'4 }
    \score { \gabc "(c4) (f)" }

    \score { g'4 }
    \score { \gabc "(c4) (g)" }

    \score { a'4 }
    \score { \gabc "(c4) (h)" }
  }

  \markup\fill-line{
    \score { b'4 }
    \score { \gabc "(c4) (i)" }

    \score { c''4 }
    \score { \gabc "(c4) (j)" }

    \score { d''4 }
    \score { \gabc "(c4) (k)" }
  }

  \markup\fill-line{
    \score { e''4 }
    \score { \gabc "(c4) (l)" }

    \score { f''4 }
    \score { \gabc "(c4) (m)" }

    ""
  }
}

\bookpart {
  \header { subtitle = "multiple notes" }

  \markup\fill-line{
    \score { \relative { c'4 d } }
    \score { \gabc "(c4) (c) (d)" }

    \score { \relative { e'4 f g } }
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

% clef changes

% accidentals

\bookpart {
  \header { subtitle = "melismata" }

  \markup\fill-line{
    \score { \relative { g'4( a) } }
    \score { \gabc "(c4) (gh)" }

    \score { \relative { g'4( a g) } }
    \score { \gabc "(c4) (ghg)" }
  }

  \markup\fill-line{
    \score { \relative { g'4 g( a) g } }
    \score { \gabc "(c4) (g) (gh) (g)" }

    ""
  }

  \markup\fill-line{
    \score { \relative { g'4( a c a b a g) } }
    \score { \gabc "(c4) (ghjhihg)" }

    ""
  }
}

\bookpart {
  \header { subtitle = "note shapes, articulations, other music elements" }

  % articulations (limited support), neumatic spaces, note shapes, adiastematic neumes (ignored)

  \markup\fill-line{
    \score { \relative { g'4 } }
    \score { \gabc "(c4) (g|vihg)" } % nabc

    ""
  }
}

\bookpart {
  \header { subtitle = "divisiones" }

  \markup\fill-line{
    \score { \relative { g'4 \bar "'" } }
    \score { \gabc "(c4) (g) (,)" }

    \score { \relative { g'4 \bar "," } }
    \score { \gabc "(c4) (g) (;)" }
  }

  \markup\fill-line{
    \score { \relative { g'4 \bar "|" } }
    \score { \gabc "(c4) (g) (:)" }

    \score { \relative { g'4 \bar "||" } }
    \score { \gabc "(c4) (g) (::)" }
  }

  % all the supported exotic forms
}

\bookpart {
  \header { subtitle = "lyrics" }

  \markup\fill-line{
    \score { \relative { g'4 } \addlyrics { La } }
    \score { \gabc "(c4) La(g)" }

    \score { \relative { g'4 g } \addlyrics { La la } }
    \score { \gabc "(c4) La(g) la(g)" }
  }

  \markup\fill-line{
    \score { \relative { g'4 g } \addlyrics { La -- la } }
    \score { \gabc "(c4) La(g)la(g)" }

    ""
  }

  \markup\fill-line{
    \score { \relative { g'4( a g) f( g) } \addlyrics { A -- men. } }
    \score { \gabc "(c4) A(ghg)men(fg)" }

    ""
  }

  \markup\fill-line{
    % lyrics with no music
    \score { \relative { g'4 \hideNotes g \unHideNotes g } \addlyrics { La "*" la } }
    \score { \gabc "(c4) La(g) *() la(g)" }

    % music with no lyrics
    \score { \relative { g'4 g g } \addlyrics { La "" la } }
    \score { \gabc "(c4) La(g) (g) la(g)" }
  }

  \markup\fill-line{
    % void syllable
    \score { \relative { g'4 \bar "" g } \addlyrics { La la } }
    \score { \gabc "(c4) La(g) () la(g)" }

    ""
  }
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
