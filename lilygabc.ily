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

tiny-note =
#(define-music-function (note) (ly:music?)
  #{ \tiny #note \normalsize #})

teeny-note =
#(define-music-function (note) (ly:music?)
  #{ \teeny #note \normalsize #})

apply-ictus =
#(define-music-function (note) (ly:music?)
  #{ #note _! #})

apply-horizontal-episema =
#(define-music-function (note) (ly:music?)
  #{ #note ^- #})

apply-virga =
#(define-music-function (side note) (boolean-or-symbol? ly:music?)
  ; TODO it would be safer to check first that Stem.length is actually overridden to 0
   #{
     \stemDown
     \once \revert Stem.length
     #(if (eq? 'right side)
       #{ \once \override NoteHead.stem-attachment = #'(0.8 . 0.3) #}
       #{ #})
     #note
     \stemNeutral
   #})
