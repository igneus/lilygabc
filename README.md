# lilygabc

lilygabc is a LilyPond library allowing direct inclusion
of chant scores encoded in the [gabc][gabc] format
in LilyPond documents:

```lilypond
\include "lilygabc.ily"

% gabc file
\score {
  \gabc-file "path/to/score.gabc"
}

% inline gabc
\score {
  \gabc "(c3) A(h)men.(h) (::)"
}
```

Internally lilygabc parses the gabc format and translates it
to LilyPond data structures, which are then engraved by LilyPond.
The Gregorio software is not involved in the process.

Creation of lilygabc was originally inspired by [this forum post](https://forum.musicasacra.com/forum/discussion/comment/256478#Comment_256478):
given the vast amount of chant transcriptions available
in the gabc format, it makes sense to provide first class support
for this music encoding format in LilyPond.

While LilyPond is not the first option for chant typesetting
in terms of output quality, there are still plenty of use cases
where a convenient way to include existing gabc scores
in a LilyPond document is useful.

## Prerequisites

*Only LilyPond is required* for using the provided LilyPond commands.

Developed with
LilyPond 2.24 (built with Guile 2.2).
Status on other versions is unknown.

---

For building the example documents:
free fonts "Junicode" and "Linux Libertine O".

For running tests:

- Guile
- Bash
- GNU Make (or compatible)
- diff (from GNU diffutils or compatible)
- optionally [Guile Library](https://www.nongnu.org/guile-lib/doc/) for colourful test results (Debian: `apt-get install guile-library`)

For building the Gregorio vs. lilygabc visual test (`make visual_tex`):

- LuaLaTex
- the [lyluatex](https://ctan.org/pkg/lyluatex) package
- Gregorio
- Guile GnuTLS bindings (Debian: `apt-get install guile-gnutls`)

## Usage

(See also:
[Examples directory](/examples),
[API Manual](/doc/manual.md))

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

Analogically to `\gabc` and `\gabc-file`
there are commands `\gabc-vaticana` and `\gabc-vaticana-file`
generating square notation.

Note that in this case it's necessary to `\include "gregorian.ly"`
in addition to lilygabc.
(lilygabc doesn't include `gregorian.ly` by default
and leaves it to the user, because `gregorian.ly` changes
global settings quite recklessly and the user should be in
control over *if* and *when* these changes are applied.)

```lilypond
% gregorian.ly MUST be included BEFORE lilygabc,
% otherwise bad things happen
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
in progress, limits of what LilyPond can do haven't been reached yet.
For supported gabc features see visual tests.
Features not covered by tests are not supported.)

### Conversion script

A script is provided, which simply dumps the music structures
built by lilygabc as either LilyPond or Scheme code.
It's useful mainly for troubleshooting purposes, providing insight
into the music structures produced internally by lilygabc.
The resulting LilyPond code is not beautiful (to put it mildly)
and sometimes not even compilable without manual clean-up.

```sh
$ ./bin/gabc2ly gabc_file.gabc            # modern notation, LilyPond syntax
$ ./bin/gabc2ly --scheme gabc_file.gabc   # modern notation, Scheme
$ ./bin/gabc2ly --vaticana gabc_file.gabc # square notation, LilyPond
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
