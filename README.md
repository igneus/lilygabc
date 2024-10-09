# lilygabc

LilyPond library which reads chant scores encoded in the
[gabc][gabc] format and translates them to LilyPond data structures,
allowing easy inclusion of gabc scores in LilyPond documents.

Inspired by [this post](https://forum.musicasacra.com/forum/discussion/comment/256478#Comment_256478):

> GABC is a description language for chant ... Chant described
> in GABC can be displayed by any software that supports it.
> ...
> Gregobase has twenty thousand entries, Neumz has about half that,
> all in GABC, and Repertorium has trained optical music recognition
> software using GABC as training data, which means the contents
> of thousands of manuscripts may soon become available as GABC.
> ...
> An alternative to GABC is a score description language
> with the ability to represent chant, polyphony and modern
> notation - and if the creator of this language has one teaspoon
> of wisdom, they will make it so existing GABC can be effortlessly
> and reliably converted into that more powerful description language.

LilyPond is not great at chant rendering.
But for those use cases when chant rendering in LilyPond
is desirable anyway, lilygabc strives to make the use of scores
in the gabc format as convenient as possible.

## Project Status & Roadmap

- [ ] render contents of gabc files in modern notation
- [ ] fundamental support for the chant notation styles supported by LilyPond
- [ ] neume detection/interpretation mimicking Gregorio (for all basic neumes and at least the most common composed ones)

## Prerequisites

Developed with
LilyPond 2.24 (built with Guile 2.2).

Status on other versions is unknown.

## Usage

Command `\gabc` loads music from a gabc string,
`\gabc-file` from a file.
The resulting scores are by default very bare-bones.
Two layout variables with settings suitable for chant in modern
notation are provided.

```lilypond
\include "lilygabc.ily"

\score {
  \gabc "(c4) (d)"

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

## Running tests

**Unit tests:**

`$ make test`

**Visual tests:**

engrave the documents in `tests/visual/`,
check results according to the instructions in the resulting documents.

## License

GNU GPL v3

[gabc]: http://gregorio-project.github.io/gabc/index.html
