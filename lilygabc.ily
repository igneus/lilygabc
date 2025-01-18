\include "gregorian-shim.ily"

% Global settings.
% Customize by overwriting values of the existing
% keys by assoc-set!.
% Re-assigning the variable as a whole will have no effect.
lilygabcGlobalSettings =
#`(
    ;; colour of lyrics marked by the <c> tag - equivalent of Gregorio's `gregoriocolor`.
    ;; Valid value is any value accepted by the \with-color LilyPond command.
    (c-tag-color . ,red)

    ;; how contents of the <v> tag should be handled:
    ;; 'print - show as text (default, because existing gabc transcriptions usually
    ;;          contain LaTeX code in <v> tags)
    ;; 'ignore - ignore the whole tag
    ;; 'as-lilypond - evaluate as LilyPond code
    (verbatim-tag . print)
    )

\include "lilypond-globals-export.ily"

#(add-to-load-path (dirname (current-filename)))
#(use-modules
  ((lilygabc) #:prefix lilygabc:)
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

%% Define names of the public functions in LilyPond syntax,
%% because some tools (like Frescobaldi) don't consider names
%% defined in Scheme modules for autocomplete and code navigation.
gabc = #lilygabc:gabc
gabc-file = #lilygabc:gabc-file
gabc-vaticana = #lilygabc:gabc-vaticana
gabc-vaticana-file = #lilygabc:gabc-vaticana-file
gly = #lilygabc:gly
gly-vaticana = #lilygabc:gly-vaticana
lilygabc-parse-gabc = #lilygabc:parse-gabc
lilygabc-parse-gly = #lilygabc:parse-gly
lilygabc-modern-music = #lilygabc:modern-music
lilygabc-vaticana-music = #lilygabc:vaticana-music
lilygabc-modern-voice = #lilygabc:modern-voice
lilygabc-vaticana-voice = #lilygabc:vaticana-voice
lilygabc-modern-lyrics = #lilygabc:modern-lyrics
lilygabc-vaticana-lyrics = #lilygabc:vaticana-lyrics
lilygabc-modern-notes = #lilygabc:modern-notes
lilygabc-vaticana-notes = #lilygabc:vaticana-notes
