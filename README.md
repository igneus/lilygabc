# lilygabc

What if LilyPond understood the gabc notation format?
What if you could have an existing gabc score rendered by LilyPond
using a single LilyPond command, no need to invoke any converter
script?

Inspired by [this post](https://forum.musicasacra.com/forum/discussion/comment/256478#Comment_256478):

    ...
    GABC is a description language for chant ... Chant described
    in GABC can be displayed by any software that supports it.
    ...
    Gregobase has twenty thousand entries, Neumz has about half that,
    all in GABC, and Repertorium has trained optical music recognition
    software using GABC as training data, which means the contents
    of thousands of manuscripts may soon become available as GABC.
    ...
    An alternative to GABC is a score description language
    with the ability to represent chant, polyphony and modern
    notation - and if the creator of this language has one teaspoon
    of wisdom, they will make it so existing GABC can be effortlessly
    and reliably converted into that more powerful description language.

LilyPond is not great at chant rendering and it will probably
never be. But for those use cases when chant rendering is desirable
anyway, lilygabc strives to make the use of scores
in the gabc format as convenient as possible.

## Prerequisites

Developed with

- LilyPond 2.24
- Guile 2.2

Status on other versions is unknown.

## Usage

```lilypond
\include "lilygabc.ily"

\score { \music-from-gabc-string "(c4) (d)" }

\score { \music-from-gabc-file "path/to/score.gabc" }
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
