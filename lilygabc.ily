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



% utility music functions used by the Scheme code,
% not part of the public interface

make-invisible-note =
#(define-music-function () ()
  #{ \hideNotes g'4 \unHideNotes #})

small-note =
#(define-music-function (note) (ly:music?)
  #{ \teeny #note \normalsize #})

apply-ictus =
#(define-music-function (note) (ly:music?)
  #{ #note _! #})

apply-horizontal-episema =
#(define-music-function (note) (ly:music?)
  #{ #note ^- #})