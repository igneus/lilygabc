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

It's necessary to add the lilygabc root directory to LilyPond
include paths:

`$ lilypond --include=path/to/lilygabc my_document.ly`

[![Example output](/doc/example.png)](/doc/example.ly)

## Running tests

**Automated tests:**

`$ make test`

**Visual tests:**

engrave the documents in `tests/visual/`,
check results according to the instructions in the resulting documents.

## License

GNU GPL v3

[gabc]: http://gregorio-project.github.io/gabc/index.html
