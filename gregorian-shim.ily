%% Features missing from LilyPond's standard "gregorian.ly"
%% required for rendering of some gabc scores

%% Clefs: gabc supports both c and f clef on any staff line
#(add-new-clef "vaticana-do0" "clefs.vaticana.do" -3 0 0)
#(add-new-clef "vaticana-fa0" "clefs.vaticana.fa" -3 0 4)
#(add-new-clef "vaticana-fa3" "clefs.vaticana.fa"  3 0 4)

%% Articulations

%% Note: "scripts.daccentus" is not suitable, as its slightly tipped to the top
#(define (lilygabc-accent-grave-stencil grob)
  (ly:stencil-scale (ly:font-get-glyph (ly:grob-default-font grob) "scripts.uaccentus") -1 1))

#(define
  gregorian-shim-script-alist
  (append
   default-script-alist
   `((lilygabcSemicircleUpper
      . ((script-stencil . (feta . ("usemicirculus" . "usemicirculus")))
         (side-relative-direction . ,DOWN)
         (quantize-position . #t)
         (avoid-slur . ignore)
         (padding . 0.20)
         (script-priority . -100)
         (direction . ,UP)))
     (lilygabcAccentGrave
      ;; TODO: don't copy-paste the whole semicircle definition, reference it and just replace stencil
      . ((stencil . ,lilygabc-accent-grave-stencil)
        ;;(script-stencil . (feta . ("daccentus" . "daccentus")))
         (side-relative-direction . ,DOWN)
         (avoid-slur . ignore)
         (padding . 0.20)
         (quantize-position . #t)
         (script-priority . -100)
         (direction . ,UP))))))

\layout {
  \context {
    \VaticanaStaff
    scriptDefinitions = #gregorian-shim-script-alist
  }
}

lilygabcSemicircleUpper = #(make-articulation 'lilygabcSemicircleUpper)
lilygabcAccentGrave = #(make-articulation 'lilygabcAccentGrave)
