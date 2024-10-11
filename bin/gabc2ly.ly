%% Converts gabc file specified in an environment variable
%% to LilyPond, prints the LilyPond code to stdout.
%% Use via the 'lilygabc-gabc2ly' shell script.

\version "2.24.0"

\include "../lilygabc.ily"

#(begin
  (use-modules (srfi srfi-98))
  (define input-file (get-environment-variable "GABC2LY_INPUT_FILE")))

\void \displayLilyMusic \gabc-file #input-file
