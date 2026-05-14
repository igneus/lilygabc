\version "2.24.1"

\include "gregorian.ly"

\include "../../lilygabc.ily" % @for-actual
\include "helper.ily" % @none

\header {
  title = "lilygabc visual tests - error cases"
}

\markup\justify{
  These test cases mostly don't produce a rendered output
  which can be checked visually, but are helpful
  for automated structural tests.
  They live in a separate file because their build yields
  LilyPond warnings (or errors) which are not welcome
  in the other visual test documents.
}

\bookpart {
  \header { subtitle = "empty/invalid scores" }

  \markup\fill-line{
    % @test empty gabc string
    \score { {} }
    \score { \gabc "" }

    % @test empty gabc string
    \score { {} }
    \score { \gabc-vaticana "" }
  }

  \markup\fill-line{
    % @test whitespace only gabc string
    \score { {} }
    \score { \gabc "   " }

    % @test whitespace only gabc string
    \score { {} }
    \score { \gabc-vaticana "   " }
  }

  \markup\fill-line{
    % @test empty music syllable
    \score { { \bar "" } }
    \score { \gabc "()" }

    % @test empty music syllable
    \score { \new VaticanaVoice { \bar "" } }
    \score { \gabc-vaticana "()" }
  }

  \markup\fill-line{
    % @test header delimiter only
    \score { {} }
    \score { \gabc "%%" }

    % @test header delimiter only
    \score { {} }
    \score { \gabc-vaticana "%%" }
  }

  \markup\fill-line{
    % @test header only
    \score { {} }
    \score { \gabc "name: Name;\nauthor: unknown;\n%%" }

    % @test header only
    \score { {} }
    \score { \gabc-vaticana "name: Name;\nauthor: unknown;\n%%" }
  }

  \markup\fill-line{
    % TODO is this expected? Shouldn't it produce LilyPond lyrics anyway?
    % @test lyrics only
    \score { {} }
    \score { \gabc "La la la" }

    % TODO is this expected? Shouldn't it produce LilyPond lyrics anyway?
    % @test lyrics only
    \score { {} }
    \score { \gabc-vaticana "La la la" }
  }
}
