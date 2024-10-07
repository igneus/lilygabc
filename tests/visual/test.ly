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

  % pitches beyond the single octave

  % flats
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

% melisma

% articulations (limited support), neumatic spaces, note shapes, adiastematic neumes (ignored)

% lyrics: single syllables

% lyrics: words

% clef positions
