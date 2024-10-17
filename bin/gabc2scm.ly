%% Converts gabc file specified in an environment variable
%% to LilyPond Scheme, prints the code to stdout.
%% Use via the 'lilygabc-gabc2scm' shell script.

\version "2.24.0"

\include "../lilygabc.ily"

#(begin
  (use-modules (srfi srfi-98))
  (define input-file (get-environment-variable "GABC2SCM_INPUT_FILE")))

\void \displayMusic \gabc-file #input-file
