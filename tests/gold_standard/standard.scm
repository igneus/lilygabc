
(make-music
  'SimultaneousMusic
  'elements
  (list (make-music
          'ContextSpeccedMusic
          'context-id
          "uniqueContext0"
          'context-type
          'Voice
          'element
          (make-music
            'SequentialMusic
            'elements
            (list (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 1 0))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 1 0))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 6))
                  (make-music
                    'SequentialMusic
                    'elements
                    (list (make-music
                            'SequentialMusic
                            'elements
                            (list (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'Dots))
                                  (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'NoteHead))
                                  (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'no-ledgers)
                                      'symbol
                                      'NoteHead))
                                  (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'Stem))
                                  (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'Accidental))
                                  (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'Rest))
                                  (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'OverrideProperty
                                      'once
                                      #t
                                      'pop-first
                                      #t
                                      'grob-value
                                      #t
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'TabNoteHead))))
                          (make-music
                            'NoteEvent
                            'duration
                            (ly:make-duration 2)
                            'pitch
                            (ly:make-pitch 0 4))))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 5))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 5))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 6))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 5))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 5))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2 1)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music 'BarEvent 'bar-type ",")
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music
                            'ArticulationEvent
                            'direction
                            -1
                            'midi-extra-velocity
                            6
                            'midi-length
                            #<procedure 7f22cb62ee00 at ice-9/eval.scm:336:13 (a b)>
                            'articulation-type
                            'staccatissimo))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 2))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music
                            'ArticulationEvent
                            'direction
                            -1
                            'midi-extra-velocity
                            6
                            'midi-length
                            #<procedure 7f22cb62ee00 at ice-9/eval.scm:336:13 (a b)>
                            'articulation-type
                            'staccatissimo))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)
                          (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 3))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 2))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music
                            'ArticulationEvent
                            'direction
                            -1
                            'midi-extra-velocity
                            6
                            'midi-length
                            #<procedure 7f22cb62ee00 at ice-9/eval.scm:336:13 (a b)>
                            'articulation-type
                            'staccatissimo)
                          (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 3))
                  (make-music 'BarEvent 'bar-type "'")
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 1))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)
                          (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 2))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 3))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1))
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 5))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2)
                    'pitch
                    (ly:make-pitch 0 5))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2 1)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music
                    'NoteEvent
                    'duration
                    (ly:make-duration 2 1)
                    'pitch
                    (ly:make-pitch 0 4))
                  (make-music 'BarEvent 'bar-type "||"))))
        (make-music
          'ContextSpeccedMusic
          'element
          (make-music
            'LyricCombineMusic
            'associated-context-type
            'Voice
            'associated-context
            "uniqueContext0"
            'element
            (make-music
              'SequentialMusic
              'elements
              (list (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "A"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "quam"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "*"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "quam"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "e"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "go"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "dé"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "de"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "ro,"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "qui"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "bí"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "be"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "rit"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "ex"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "e"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "a,"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "non"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "sí"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "ti"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "et"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'articulations
                      (list (make-music 'HyphenEvent))
                      'text
                      "un"
                      'duration
                      (ly:make-duration 2))
                    (make-music
                      'LyricEvent
                      'text
                      "quam."
                      'duration
                      (ly:make-duration 2))
                    (make-music 'CompletizeExtenderEvent))))
          'property-operations
          '()
          'context-type
          'Lyrics
          'create-new
          #t)))

