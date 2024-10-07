\version "2.24.1"

\include "../../lilygabc.ily"

\header {
  title = "lilygabc visual tests"
}

\bookpart {
  \header { subtitle = "single note" }

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

    ""

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
