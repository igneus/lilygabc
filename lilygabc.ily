#(load "lilygabc.scm")

lilygabcModernGregorianLayout = \layout {
  \override Score.TimeSignature.stencil = ##f
  \set Timing.timing = ##f
}

lilygabcModernGregorianStemlessLayout = \layout {
  \lilygabcModernGregorianLayout

  \override Stem.length = 0
}
