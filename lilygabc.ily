#(add-to-load-path (dirname (current-filename)))
#(load "lilygabc.scm")

lilygabcModernGregorianLayout = \layout {
  \override Score.TimeSignature.stencil = ##f
  \set Timing.timing = ##f
}

lilygabcModernGregorianStemlessLayout = \layout {
  \lilygabcModernGregorianLayout

  \override Stem.length = 0
}

make-invisible-note =
#(define-music-function () ()
  #{ \hideNotes g'4 \unHideNotes #})
