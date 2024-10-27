# lilygabc

LilyPond library which reads chant scores encoded in the
[gabc][gabc] format and translates them to LilyPond data structures,
allowing easy inclusion of gabc scores in LilyPond documents.
This kind of easy:

```lilypond
\include "lilygabc.ily"

\score {
  \gabc-file "path/to/score.gabc"
}
```

Originally inspired by [this forum post](https://forum.musicasacra.com/forum/discussion/comment/256478#Comment_256478).
Given the vast amount of chant transcriptions available
in the gabc format, it makes sense to provide first class support
for this music encoding format in LilyPond.

While LilyPond is not the first option for chant typesetting
in terms of output quality, there are still plenty of use cases
where a convenient way to include existing gabc scores
is useful.

## Project Status & Roadmap

*Early stage of development, several fundamental features still missing.*

- [ ] render contents of gabc files in modern notation
- [ ] fundamental support for the chant notation styles supported by LilyPond
- [ ] neume detection/interpretation mimicking Gregorio (for all basic neumes and at least the most common composed ones)
- [ ] (yet another) conversion script converting gabc to clean LilyPond
  code - because easy inclusion of gabc scores in LilyPond documents
  is nice, but sometimes manual tweaking is necessary

## Prerequisites

*Only LilyPond is required* for using the provided LilyPond commands.

Developed with
LilyPond 2.24 (built with Guile 2.2).
Status on other versions is unknown.

---

For running tests:

- Guile
- Bash
- GNU Make (or compatible)
- diff (from GNU diffutils or compatible)

For building the Gregorio vs. lilygabc visual test (`make visual_tex`):

- LuaLaTex
- the lyluatex package
- Gregorio

## Usage

lilygabc supports rendering gabc scores both in modern notation
and in square notation.

### Modern notation

The resulting scores are by default very bare-bones.
Two layout variables with settings suitable for chant in modern
notation are provided.
It's suggested to include one of them either in the global
`\layout{ }` block, or in the layout block of each lilygabc score.

```lilypond
\include "lilygabc.ily"

\score {
  % gabc music from an inline string
  \gabc
    "(c4) Ju(e)bi(f)lá(g')te(f) De(d_f)o(f'_) (,)
    o(f)mnis(f) ter(e_f)ra,(d) al(f)le(fg)lú(e.)ia.(e.) (::)
    E(h) u(g) o(h) u(ih) a(gf) e.(e.) (::)"

  \layout {
    % use a provided layout variable for reasonable default settings
    \lilygabcModernGregorianLayout
  }
}

\score {
  % load a gabc file
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

### Square notation

Analogically to commands `\gabc` and `\gabc-file`
there are commands `\gabc-vaticana` and `\gabc-vaticana-file`
generating square notation.

Note that in this case it's necessary to `\include "gregorian.ly"`
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

### Conversion script

A script is provided, which simply dumps the music structures
built by lilygabc as either LilyPond or Scheme code.
Be warned that the code thus produced is rather ugly.

```sh
$ ./bin/gabc2ly gabc_file.gabc
$ ./bin/gabc2ly --scheme gabc_file.gabc
$ ./bin/gabc2ly --vaticana gabc_file.gabc
```

## Running tests

**Automated tests:**

`$ make test`

This command executes several layers of tests:

- unit tests of the core Scheme modules
- integration tests checking music structures produced by lilygabc
  against expected results coded manually in LilyPond
- regression tests checking music structures of real-life scores
  against structures previously saved

**Visual tests:**

engrave the documents in `tests/visual/`,
check results according to instructions in the resulting documents.

## License

GNU GPL v3

[gabc]: http://gregorio-project.github.io/gabc/index.html
