# lilygabc

LilyPond library which reads chant scores encoded in the
[gabc][gabc] format and translates them to LilyPond data structures,
allowing easy inclusion of gabc scores in LilyPond documents.

Originally inspired by [this forum post](https://forum.musicasacra.com/forum/discussion/comment/256478#Comment_256478).
Given the vast amount of chant transcriptions available
in the gabc format, it makes sense to provide first class support
for this music encoding format in LilyPond.

While LilyPond is not the first option for chant typesetting
in terms of output quality, there are still plenty of use cases
where a convenient way to include existing gabc scores
is useful.

## Project Status & Roadmap

*Only rendering in modern notation supported so far, several fundamental features still missing.*

- [ ] render contents of gabc files in modern notation
- [ ] fundamental support for the chant notation styles supported by LilyPond
- [ ] neume detection/interpretation mimicking Gregorio (for all basic neumes and at least the most common composed ones)

## Prerequisites

Only LilyPond is required for using the provided LilyPond commands.

Developed with
LilyPond 2.24 (built with Guile 2.2).
Status on other versions is unknown.

For running tests:

- Guile
- Bash
- GNU Make (or compatible)

For building the Gregorio vs. lilygabc visual test (`make visual_tex`):

- LuaLaTex
- the lyluatex package
- Gregorio

## Usage

Command `\gabc` loads music from a gabc string,
`\gabc-file` from a file.

The resulting scores are by default very bare-bones.
Two layout variables with settings suitable for chant in modern
notation are provided.
It's suggested to include one of them either in the global
`\layout{ }` block, or in the layout block of each lilygabc score.

```lilypond
\include "lilygabc.ily"

\score {
  \gabc
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
    E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"

  \layout {
    \lilygabcModernGregorianLayout
  }
}

\score {
  \gabc-file "path/to/score.gabc"

  \layout {
    \lilygabcModernGregorianStemlessLayout
  }
}
```

When engraving the document
it's necessary to add the lilygabc root directory to LilyPond
include paths:

`$ lilypond --include=path/to/lilygabc my_document.ly`

[![Example output: modern notation](/doc/example.png)](/doc/example.ly)

In order to render gabc scores in square notation,
use commands `\gabc-vaticana` and `\gabc-vaticana-file`.

Note that in this case you have to `\include "gregorian.ly"`
in addition to lilygabc.
(lilygabc doesn't include `gregorian.ly` by default
and leaves it to the user, because `gregorian.ly` changes
global settings quite recklessly and the user should be in
control over *if* and *when* these changes are applied.)

```lilypond
% If the \lilygabcVaticanaLayout layout variable is used,
% it's important to include gregorian.ly *before* lilygabc
\include "gregorian.ly"

\include "lilygabc.ily"

\score {
  \gabc-vaticana
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
    E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"

  \layout {
    \lilygabcVaticanaLayout
  }
}
```

[![Example output: square notation](/doc/vaticana-example.png)](/doc/vaticana-example.ly)

(The square notation support in lilygabc is still very much a work
in progress, limits of what LilyPond can do haven't been reached yet.)

## Running tests

**Automated tests:**

`$ make test`

**Visual tests:**

engrave the documents in `tests/visual/`,
check results according to the instructions in the resulting documents.

## License

GNU GPL v3

[gabc]: http://gregorio-project.github.io/gabc/index.html
