\version "2.24.1"

\include "gregorian.ly"
\include "../../lilygabc.ily"
\include "helper.ily"

\header {
  title = "lilygabc tests - the lower level API"
}

% Test that composed calls of the lower level API produce the same results
% as the high level API.
\markup\fill-line{
  % @test
  \score { \gabc "(c4) (g)" }
  \score { \lilygabc-modern-music \lilygabc-parse-gabc "(c4) (g)" }

  % @test
  \score { \gabc-vaticana "(c4) (g)" }
  \score { \lilygabc-vaticana-music \lilygabc-parse-gabc "(c4) (g)" }
}

\markup\fill-line{
  % @test equal gly and gabc produce equal music
  \score { \lilygabc-modern-music \lilygabc-parse-gabc "(c4) (g)" }
  \score { \lilygabc-modern-music \lilygabc-parse-gly "c4 g" }

  "" ""
}
