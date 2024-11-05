# lilygabc API Manual

This document describes all functions provided by lilygabc.
For basic setup and usage instructions see the [README](/).

lilygabc exposes two levels of a public interface.
High-level API serves all usual use-cases.
Low-level API is for scenarios where more control is needed,
like loading a gabc file only once and generating several
variants of music output, or transforming
the parsed gabc structure before letting lilygabc translate
it to LilyPond music.

## High-level API

Each function orchestrates the whole process from loading
gabc to producing LilyPond music.

All of them accept a single mandatory string argument
(source of the data to be loaded)
and an optional [association list](https://lilypond.org/doc/v2.24/Documentation/extending/scheme-compound-data-types#association-lists-_0028alists_0029)
which can be used to pass options.

```lilypond
\score { \gabc "(c4) A(ghg)men(fg)" }

% using the optional argument
\score {
  \gabc #'((produce . voice)
           (voice-id . "my_voice_1"))
        "(c4) A(ghg)men(fg)"
  \new Lyrics \lyricsto "my_voice_1" { La -- la }
}
```

Supported options (the key is always a symbol):

- `parse-as` - value `gly` (symbol) makes the input be handled
  as [gly][gly] *music line(s)* rather than gabc.
  A gly music line is basically gabc minus lyrics and parentheses.
  Very convenient when typing music alone.
- `produce` - supported values are `voice`, `lyrics`, `notes` (symbol).
  This option makes the function return a partial music structure
  (a LilyPond Voice/Lyrics/bare notes) rather than the full
  music structure containing all information available in the source
  data (usually a Voice + associated Lyrics).
  This may be useful when using data from gabc as part of a larger
  LilyPond construct rather than as a separate score.
- `voice-id` - sets the voice ID (string),
  which is useful mainly for attaching additional lyrics
  to the same voice in LilyPond code.

The functions are a matrix combining
input format (gabc / gly),
notation style (modern / square notation, which is called Vaticana in the LilyPond world)
and input source (string / file).

- `\gabc (options) input` - string gabc input -> modern notation
- `\gabc-file (options) path` - file gabc input -> modern notation

- `\gabc-vaticana (options) input` - string gabc input -> square notation
- `\gabc-vaticana-file (options) path` - file gabc input -> square notation

- `\gly (options) input` - string gly input -> modern notation
- `\gly-vaticana (options) input` - string gly input -> square notation

(There is no gly file input, because lilygabc doesn't support
the complete gly language. The idea behind partial gly support
is to provide a convenient way to enter square notation music
to be then combined with lyrics in the standard LilyPond syntax.)

## Low-level API

The low level API exposes input parsing and music production
as separate functions, allowing parsed gabc data to be used repeatedly
to produce output of different kinds or
to be transformed before producing some sort of output.

### Parsing input

Both functions accept input as string and return parsed
data as a list-based tree structure.

(For details of the data structure see the `lilygabc gabc` module
and its unit tests. The data structure is currently not official part
of the public interface and may change without warning.)

- `\lilygabc-parse-gabc input`
- `\lilygabc-parse-gly input`

### Producing output

#### Complete music

The functions accept a single mandatory argument - the data structure
produced by the parsing functions above.

Optionally an association list of options may be passed.
For supported options see the high-level API above.

- `\lilygabc-modern-music options score`
- `\lilygabc-vaticana-music options score`

#### Voice

`context-id` is a unique ID of the voice,
`score` is the data structure produced by parsing functions.

- `\lilygabc-modern-voice context-id score`
- `\lilygabc-vaticana-voice context-id score`

#### Bare notes

Bare notes, not wrapped in a voice.
Be warned that while bare modern notation notes usually work
as expected, square notation notes, unless wrapped in a `VaticanaVoice`,
won't be recognized as such and will be rendered in modern notation.

- `\lilygabc-modern-notes score`
- `\lilygabc-vaticana-notes score`

#### Lyrics

[gly]: https://github.com/igneus/gly
