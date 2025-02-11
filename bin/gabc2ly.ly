%% Converts gabc file specified in an environment variable
%% to LilyPond or Scheme, prints the code to stdout.
%% Use via the 'gabc2ly' shell script.

\version "2.24.0"

#(begin
  (use-modules (srfi srfi-98))
  (define input-file (get-environment-variable "GABC2LY_INPUT_FILE"))
  (define as-vaticana (get-environment-variable "GABC2LY_VATICANA"))
  (define as-scheme (get-environment-variable "GABC2LY_SCHEME")))

%% Conditional \include : neither of
%%
%%   #(when as-vaticana #{ \include "gregorian.ly" #})
%%   #(when as-vaticana (include "gregorian.ly"))
%%
%% works.
#(when as-vaticana
  (ly:parser-include-string "\\include \"gregorian.ly\""))

\include "../lilygabc.ily"

#(begin
  (define render-fn (if as-vaticana gabc-vaticana-file gabc-file))
  (define display-fn (if as-scheme displayMusic displayLilyMusic)))

\void \display-fn \render-fn #input-file
