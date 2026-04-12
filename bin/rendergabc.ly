%% Renders gabc file specified in an environment variable.
%% Use via the 'rendergabc' shell script.

\version "2.24.0"

#(begin
  (use-modules (srfi srfi-98))
  (define input-file (get-environment-variable "RENDERGABC_INPUT_FILE"))
  (define as-vaticana (get-environment-variable "RENDERGABC_VATICANA")))

%% Conditional \include : neither of
%%
%%   #(when as-vaticana #{ \include "gregorian.ly" #})
%%   #(when as-vaticana (include "gregorian.ly"))
%%
%% works.
#(when as-vaticana
  (ly:parser-include-string "\\include \"gregorian.ly\""))

\include "../lilygabc.ily"

emptyLayout = \layout {}

#(begin
  (define render-fn (if as-vaticana gabc-vaticana-file gabc-file))
  (define useLayout (if as-vaticana emptyLayout lilygabcModernGregorianStemlessLayout)))

\score {
  \render-fn #input-file
  \layout {
    \useLayout
    \set Score.forbidBreakBetweenBarLines = ##f
  }
}
