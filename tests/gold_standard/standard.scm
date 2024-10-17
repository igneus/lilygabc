
(make-music
  'SimultaneousMusic
  'elements
  (list (make-music
          'ContextSpeccedMusic
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
                    'pitch
                    (ly:make-pitch 1 0)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 1 0)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 6)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
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
                            (ly:make-pitch 0 4))
                          (make-music
                            'SequentialMusic
                            'elements
                            (list (make-music
                                    'ContextSpeccedMusic
                                    'context-type
                                    'Bottom
                                    'element
                                    (make-music
                                      'RevertProperty
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
                                      'RevertProperty
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
                                      'RevertProperty
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
                                      'RevertProperty
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
                                      'RevertProperty
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
                                      'RevertProperty
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
                                      'RevertProperty
                                      'grob-property-path
                                      (list 'transparent)
                                      'symbol
                                      'TabNoteHead))))))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 5)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 5)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 6)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 5)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 5)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2 1))
                  (make-music 'BarEvent 'bar-type ",")
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2))
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
                            #<procedure 7f05a8256320 at ice-9/eval.scm:336:13 (a b)>
                            'articulation-type
                            'staccatissimo))
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 2)
                    'duration
                    (ly:make-duration 2))
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
                            #<procedure 7f05a8256320 at ice-9/eval.scm:336:13 (a b)>
                            'articulation-type
                            'staccatissimo))
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 3)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)
                          (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 2)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
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
                            #<procedure 7f05a8256320 at ice-9/eval.scm:336:13 (a b)>
                            'articulation-type
                            'staccatissimo)
                          (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto))
                    'pitch
                    (ly:make-pitch 0 3)
                    'duration
                    (ly:make-duration 2))
                  (make-music 'BarEvent 'bar-type "'")
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 1)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 2)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)
                          (make-music
                            'ArticulationEvent
                            'direction
                            1
                            'articulation-type
                            'tenuto)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 3)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction -1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 5)
                    'duration
                    (ly:make-duration 2)
                    'articulations
                    (list (make-music 'SlurEvent 'span-direction 1)))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 5)
                    'duration
                    (ly:make-duration 2))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2 1))
                  (make-music
                    'NoteEvent
                    'pitch
                    (ly:make-pitch 0 4)
                    'duration
                    (ly:make-duration 2 1))
                  (make-music 'BarEvent 'bar-type "||")))
          'context-type
          'Voice
          'context-id
          "uniqueContext0")
        (make-music
          'ContextSpeccedMusic
          'create-new
          #t
          'context-type
          'Lyrics
          'property-operations
          '()
          'element
          (make-music
            'LyricCombineMusic
            'element
            (make-music
              'SequentialMusic
              'elements
              (list (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "A"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "quam")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "*")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "quam")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "e"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "go")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "dé"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "de"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "ro,")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "qui")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "bí"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "be"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "rit")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "ex")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "e"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "a,")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "non")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "sí"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "ti"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "et")
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "un"
                      'articulations
                      (list (make-music 'HyphenEvent)))
                    (make-music
                      'LyricEvent
                      'duration
                      (ly:make-duration 2)
                      'text
                      "quam.")))
            'associated-context
            "uniqueContext0"
            'associated-context-type
            'Voice))))

