\version "2.24.1"

\header {
  title = "Psalmus 117 (116)"
  subtitle = "(Nova Vulgata)"
}

\include "gregorian.ly"
\include "../lilygabc.ily"

#(set-global-staff-size 32)

\layout {
  \set VaticanaStaff.forbidBreakBetweenBarLines = ##t
  \override VaticanaStaff.StaffSymbol.color = "black"
  \override VaticanaStaff.NoteHead.whiteout = ##t

  \override VaticanaLyrics.LyricText.self-alignment-X = #LEFT
  \override VaticanaLyrics.LyricText.font-size = #-2

  ragged-last = ##f
}

\paper {
  #(set-paper-size "a5landscape")

  #(define fonts
     (set-global-fonts
      #:roman "Junicode"
      #:factor (/ staff-height pt 25)))
}

\score {
  <<
    \gabc-vaticana
      "(c4) Lau(f)dá(gh)te(h) Dóminum,(hrhrhr) om(ixir1)(hr)nes(h) gen(gr1)(hr)tes;(h) *(;)
      collaudáte eum,(hrhrhr) om(g)nes(f) pó(ghr1)pu(gr)li.(gvFED) (::)"

    \new VaticanaLyrics \lyricsto "uniqueContext0" { % TODO voice ID relies on lilygabc internals
      Quó -- ni -- am "confirmáta est super nos miseri" -- cór -- di -- a e -- \skip 1 ius, "*"
      "et véritas Dómini ma" -- net in æ -- tér -- num.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" { % TODO voice ID relies on lilygabc internals
      Gló -- ri -- a \skip 1 Pa -- tri, et Fí -- li -- o, "*"
      "et Spirí" -- tu -- i Sanc -- \skip 1 to.
    }
    \new VaticanaLyrics \lyricsto "uniqueContext0" { % TODO voice ID relies on lilygabc internals
      Si -- cut e -- "rat in princípio, et" nunc, \skip 1 et sem -- \skip 1 per, "*"
      "et in sǽcula sæcu" -- ló -- rum. A -- \skip 1 men.
    }
  >>
}
