\version "2.24.1"

\paper {
  markup-markup-spacing.padding = #2
}

% https://lsr.di.unimi.it/LSR/Snippet?id=726
bgcolor =
#(define-music-function (color) (string?)
 #{\override Staff.StaffSymbol.stencil = $(lambda (grob)
    (let* ((staff (ly:staff-symbol::print grob))
           (X-ext (ly:stencil-extent staff X))
           (Y-ext (ly:stencil-extent staff Y)))
         (set! Y-ext (cons
            (- (car Y-ext) 2)
            (+ (cdr Y-ext) 2)))
         (ly:grob-set-property! grob 'layer -10)
         (ly:stencil-add
           (ly:make-stencil (list 'color (eval-string color)
               (ly:stencil-expr (ly:round-filled-box X-ext Y-ext 0))
               X-ext Y-ext))
           staff)))
#})

% mark an example as a known failing test
xfail = { \bgcolor "(x11-color 'Gold)" }

% mark an example as a work currently in progress -
% unlike xfails, these examples should not be committed to git
xtodo = \layout {
  \bgcolor "magenta"
  \context {
    \VaticanaStaff
    % gregorian.ly's red staff lines are unreadable on magenta
    \override StaffSymbol.color = #grey
    \override LedgerLineSpanner.color = #grey
  }
}

wontfix = { \bgcolor "(x11-color 'grey)" }
