\version "2.24.1"

%% Features missing from LilyPond's standard "gregorian.ly"
%% required for rendering of some gabc scores

#(add-to-load-path (dirname (current-filename)))

#(use-modules
  ((lilygabc util) #:select (alist-merge)))

%% Clefs: gabc supports both c and f clef on any staff line

#(add-new-clef "vaticana-do0" "clefs.vaticana.do" -3 0 0)
#(add-new-clef "vaticana-fa0" "clefs.vaticana.fa" -3 0 4)
#(add-new-clef "vaticana-fa3" "clefs.vaticana.fa"  3 0 4)

%% Articulations

%% Note: "scripts.daccentus" is not suitable, as it's slightly tipped to the top
#(define (lilygabc-accent-grave-stencil grob)
  (ly:stencil-scale (ly:font-get-glyph (ly:grob-default-font grob) "scripts.uaccentus") -1 1))

#(define
  gregorian-shim-script-alist
  (append
   default-script-alist
   `((lilygabcSemicircleUpper
      . ,(alist-merge
          (assoc-ref default-script-alist 'semicirculus)
          '((script-stencil . (feta . ("usemicirculus" . "usemicirculus"))))))
     (lilygabcAccentGrave
      . ,(append
          `((stencil . ,lilygabc-accent-grave-stencil))
          (alist-delete 'script-stencil (assoc-ref default-script-alist 'accentus)))))))

\layout {
  \context {
    \VaticanaStaff
    scriptDefinitions = #gregorian-shim-script-alist
  }
}

lilygabcSemicircleUpper = #(make-articulation 'lilygabcSemicircleUpper)
lilygabcAccentGrave = #(make-articulation 'lilygabcAccentGrave)

%% Divisiones

% Dominican "quarter bars"; numbering mimicks gabc, c.f. gabc ";1", ";2" etc.
lilygabcDivisioDominicanQI = {
  \once \override Staff.BarLine.bar-extent = #'(-1.5 . 0)
  \bar "|"
}
lilygabcDivisioDominicanQII = {
  \once \override Staff.BarLine.bar-extent = #'(-2 . -0.5)
  \bar "|"
}
lilygabcDivisioDominicanQIII = {
  \once \override Staff.BarLine.bar-extent = #'(-0.5 . 1)
  \bar "|"
}
lilygabcDivisioDominicanQIV = {
  \once \override Staff.BarLine.bar-extent = #'(-1 . 0.5)
  \bar "|"
}
lilygabcDivisioDominicanQV = {
  \once \override Staff.BarLine.bar-extent = #'(0.5 . 2)
  \bar "|"
}
lilygabcDivisioDominicanQVI = {
  \once \override Staff.BarLine.bar-extent = #'(0 . 1.5)
  \bar "|"
}
