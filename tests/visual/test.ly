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
    \score { \music-from-gabc-string "(c4) (a)" }

    \score { b4 }
    \score { \music-from-gabc-string "(c4) (b)" }
  }

  \markup\fill-line{
    \score { c'4 }
    \score { \music-from-gabc-string "(c4) (c)" }

    \score { d'4 }
    \score { \music-from-gabc-string "(c4) (d)" }

    \score { e'4 }
    \score { \music-from-gabc-string "(c4) (e)" }
  }

  \markup\fill-line{
    \score { f'4 }
    \score { \music-from-gabc-string "(c4) (f)" }

    \score { g'4 }
    \score { \music-from-gabc-string "(c4) (g)" }

    \score { a'4 }
    \score { \music-from-gabc-string "(c4) (h)" }
  }

  \markup\fill-line{
    \score { b'4 }
    \score { \music-from-gabc-string "(c4) (i)" }

    \score { c''4 }
    \score { \music-from-gabc-string "(c4) (j)" }

    \score { d''4 }
    \score { \music-from-gabc-string "(c4) (k)" }
  }

  \markup\fill-line{
    \score { e''4 }
    \score { \music-from-gabc-string "(c4) (l)" }

    \score { f''4 }
    \score { \music-from-gabc-string "(c4) (m)" }

    ""
  }
}

\bookpart {
  \header { subtitle = "multiple notes" }

  \markup\fill-line{
    \score { \relative { c'4 d } }
    \score { \music-from-gabc-string "(c4) (c) (d)" }

    \score { \relative { e'4 f g } }
    \score { \music-from-gabc-string "(c4) (e) (f) (g)" }
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
    \score { \music-from-gabc-string #(string-append "(c4)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { c'4 a'' } }
    \score { \music-from-gabc-string #(string-append "(c3)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { e'4 c'' } }
    \score { \music-from-gabc-string #(string-append "(c2)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { g'4 e'' } }
    \score { \music-from-gabc-string #(string-append "(c1)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { d4 b'' } }
    \score { \music-from-gabc-string #(string-append "(f4)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { f4 d'' } }
    \score { \music-from-gabc-string #(string-append "(f3)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { a4 f'' } }
    \score { \music-from-gabc-string #(string-append "(f2)" allGabcNotes) }
  }

  \markup\line{
    \score { \relative { c'4 a'' } }
    \score { \music-from-gabc-string #(string-append "(f1)" allGabcNotes) }
  }
}

% clef changes

% accidentals

\bookpart {
  \header { subtitle = "melismata" }

  \markup\fill-line{
    \score { \relative { g'4( a) } }
    \score { \music-from-gabc-string "(c4) (gh)" }

    \score { \relative { g'4( a g) } }
    \score { \music-from-gabc-string "(c4) (ghg)" }
  }

  \markup\fill-line{
    \score { \relative { g'4 g( a) g } }
    \score { \music-from-gabc-string "(c4) (g) (gh) (g)" }

    ""
  }

  \markup\fill-line{
    \score { \relative { g'4( a c a b a g) } }
    \score { \music-from-gabc-string "(c4) (ghjhihg)" }

    ""
  }
}

% articulations (limited support), neumatic spaces, note shapes, adiastematic neumes (ignored)

\bookpart {
  \header { subtitle = "divisiones" }

  \markup\fill-line{
    \score { \relative { g'4 \bar "'" } }
    \score { \music-from-gabc-string "(c4) (g) (,)" }

    \score { \relative { g'4 \bar "," } }
    \score { \music-from-gabc-string "(c4) (g) (;)" }
  }

  \markup\fill-line{
    \score { \relative { g'4 \bar "|" } }
    \score { \music-from-gabc-string "(c4) (g) (:)" }

    \score { \relative { g'4 \bar "||" } }
    \score { \music-from-gabc-string "(c4) (g) (::)" }
  }

  % all the supported exotic forms
}

% lyrics: single syllables

% lyrics: words

\bookpart {
  \header { subtitle = "real-life score" }

  \markup\justify{
    (There is no hand-coded expected result,
    all scores showcase lilygabc chant rendering using the provided styles.)
  }
  \score {
    \music-from-gabc-string
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
}
