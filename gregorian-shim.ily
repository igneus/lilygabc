%% Features missing from LilyPond's standard "gregorian.ly"
%% required for rendering of some gabc scores

%% Clefs: Gregorio supports both c and f clef on any staff line
#(add-new-clef "vaticana-do0" "clefs.vaticana.do" -3 0 0)
#(add-new-clef "vaticana-fa0" "clefs.vaticana.fa" -3 0 4)
#(add-new-clef "vaticana-fa3" "clefs.vaticana.fa"  3 0 4)
