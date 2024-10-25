\version "2.24.1"

\include "gregorian.ly"
\include "../../gregorian-shim.ily"
% do NOT include lilygabc.ily

\header {
  title = "lilygabc visual tests - gregorian-shim.ily"
}

\markup\justify{
  This document checks that \typewriter{gregorian-shim.ily}
  doesn't secretly rely on any global definitions of lilygabc
  and can be used on its own.
}

\markup{
  Custom clefs
}

\score { \new VaticanaVoice { \clef "vaticana-do0" a' } }
\score { \new VaticanaVoice { \clef "vaticana-fa0" a } }
\score { \new VaticanaVoice { \clef "vaticana-fa3" a, } }

\markup{
  Custom articulations
}

\score { \new VaticanaVoice {
  \clef "vaticana-do3"
  g\lilygabcSemicircleUpper
  g\lilygabcAccentGrave
} }
