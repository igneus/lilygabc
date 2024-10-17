#(add-to-load-path (dirname (current-filename)))
#(load "lilygabc.scm")
#(load "vaticana.scm")

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
  #{ \once \hideNotes g'4 #})

%% N.B.: all functions applying features to a note
%% must have that note as their last expression
%% (not e.g. a state-resetting command).
%% Otherwise a subsequent application of another function
%% on the result of the function call
%% results in an unattached ArticulationEvent.
tiny-note =
#(define-music-function (note) (ly:music?)
  #{ \once \tiny #note #})

teeny-note =
#(define-music-function (note) (ly:music?)
  #{ \once \teeny #note #})

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
     \once \stemDown
     \once \revert Stem.length
     #(if (eq? 'right side)
       #{ \once \override NoteHead.stem-attachment = #'(0.8 . 0.3) #})
     #note
   #})

key-flat = { \key f \major }
key-natural = { \key c \major }
