#(add-to-load-path (dirname (current-filename)))
#(load "lilygabc.scm")
#(load "vaticana.scm")

\include "gregorian-shim.ily"

%
% custom articulation marks
%

#(define lilygabc:modern-gregorian:ictus-stencil
  (make-line-stencil 0.1 0 0 0 0.7))

#(define lilygabc-modern-gregorian-script-alist
  (map
   (lambda (x)
    (if (eq? 'staccatissimo (car x))
     ;; Defining a new custom articulation would be semantically cleaner,
     ;; but the advantage of redefining staccatissimo is that it provides
     ;; a default look, more or less conveying the information,
     ;; even if the custom articulation definitions are not applied.
     `(staccatissimo
       . ((stencil . ,lilygabc:modern-gregorian:ictus-stencil)
          (toward-stem-shift-in-column . 0.0)
          (padding . 0.4)
          (avoid-slur . around)
          (direction . ,DOWN)))
     x))
   default-script-alist))



%
% layout variables
% providing decent looks for lilygabc scores
%

lilygabcModernGregorianLayout = \layout {
  \set Timing.timing = ##f

  \context {
    \Score
    scriptDefinitions = #lilygabc-modern-gregorian-script-alist
  }
  \context {
    \Staff
    \remove Time_signature_engraver
  }
}

lilygabcModernGregorianStemlessLayout = \layout {
  \lilygabcModernGregorianLayout

  \override Stem.length = 0
}

% Nota bene: if you plan to use this layout variable,
% order in which lilygabc.ily and gregorian.ly are \included
% has serious consequences for the contents of the variable,
% as described in http://lilypond.org/doc/v2.24/Documentation/notation/the-layout-block
% The strongly suggested order is to include gregorian.ly first.
lilygabcVaticanaLayout = \layout {
  % staff and lyrics font size to match the modern notation
  % when included in the same document
  #(layout-set-staff-size 26)

  % TODO where is the default global setting which makes VaticanaLyrics so tiny?
  \override VaticanaLyrics.LyricText.font-size = #-0.8
}



% Utility music functions used by the Scheme code.
% Not part of the public interface.

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

apply-vaticana-ictus =
#(define-music-function (note) (ly:music?)
  #{ #note \ictus #})

% TODO probably merge with the function below
apply-horizontal-episema =
#(define-music-function (note) (ly:music?)
  #{ #note ^- #})

apply-single-note-episema =
#(define-music-function (note) (ly:music?)
  #{ #note \episemInitium \episemFinis #})

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
