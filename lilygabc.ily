\include "gregorian-shim.ily"
\include "lilypond-globals-export.ily"

#(add-to-load-path (dirname (current-filename)))
#(use-modules
  (lilygabc)
  ((lilygabc util) #:prefix util:))

%
% custom articulation marks
%

#(define lilygabc:modern-gregorian:ictus-stencil
  (make-line-stencil 0.1 0 0 0 0.7))

#(define lilygabc-modern-gregorian-script-alist
  (util:alist-merge
   default-script-alist
   `((staccatissimo
      . ((stencil . ,lilygabc:modern-gregorian:ictus-stencil)
         (toward-stem-shift-in-column . 0.0)
         (padding . 0.4)
         (avoid-slur . around)
         (direction . ,DOWN))))))



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
  \context {
    \Voice
    \consists Episema_engraver
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

  % default value of -4 is set in engraver-init.ly
  \override VaticanaLyrics.LyricText.font-size = #-0.8
}
