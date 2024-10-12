\version "2.24.1"

\include "../../lilygabc.ily"

\header {
  title = "lilygabc visual tests"
}

\paper {
  markup-markup-spacing.padding = #2
}

% https://lsr.di.unimi.it/LSR/Snippet?id=726
bgcolor =
#(define-music-function (color) (string?)
 #{\override Staff.StaffSymbol.stencil = $(lambda (grob)
    (let* ((staff (ly:staff-symbol::print grob))
           (X-ext (ly:stencil-extent staff X))
           (Y-ext (ly:stencil-extent staff Y)))
         (set! Y-ext (cons
            (- (car Y-ext) 2)
            (+ (cdr Y-ext) 2)))
         (ly:grob-set-property! grob 'layer -10)
         (ly:stencil-add
           (ly:make-stencil (list 'color (eval-string color)
               (ly:stencil-expr (ly:round-filled-box X-ext Y-ext 0))
               X-ext Y-ext))
           staff)))
#})

% mark an example as a known failing test
xfail = { \bgcolor "(x11-color 'Gold)" }

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
    showcasing features not yet supported by lilygabc or known bugs.
  }
}

\bookpart {
  \header { subtitle = "single note" }

  \markup\fill-line{
    \score { a4 }
    \score { \gabc "(c4) (a)" }

    \score { b4 }
    \score { \gabc "(c4) (b)" }

    \score { c'4 }
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

    \score { \relative { g'4( \teeny f e d) \normalsize g } }
    \score { \gabc "(c4) (gFED) (g)" } % punctum inclinatum
  }

  \markup\fill-line{
    \score { \relative { g'4( \teeny f) } }
    \score { \gabc "(c4) (gf~)" } % diminutive liquescence

    \score { \relative { g'4. g4 } }
    \score { \gabc "(c4) (g.) (g)" } % punctum mora
  }

  \markup\fill-line{
    \score { \relative { g'4_! } }
    \score { \gabc "(c4) (g')" } % ictus / vertical episema

    \score { \relative { g'4^- } }
    \score { \gabc "(c4) (g_)" } % horizontal episema
  }

  \markup\fill-line{
    \score { \relative { g'4.^-_! } }
    \score { \gabc "(c4) (g._')" } % all supported articulations at once

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

  \markup\fill-line{
    \score { \relative { g'4 \breathe } }
    \score { \gabc "(c4) (g) (`)" }

    "" ""
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

    "" ""
  }

  \markup\fill-line{
    % lyrics under a divisio:
    % LilyPond doesn't support that -> use an invisible note as a workaround
    \score { \relative { g'4 \hideNotes g \unHideNotes \bar "'" g } \addlyrics { La "*" la } }
    \score { \gabc "(c4) La(g) *(,) la(g)" }

    \score { \relative { g'4 \hideNotes g \unHideNotes \bar "|" g } \addlyrics { La T.P. la } }
    \score { \gabc "(c4) La(g) T.P.(:) la(g)" }
  }

  \markup\fill-line{
    \score { \relative { g'4( a g \bar "'" g c) c } \addlyrics { La la } }
    \score { \gabc "(c4) La(ghg,gj) la(j)" } % divisio inside a music syllable

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
