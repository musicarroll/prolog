# Musicomp

Musicomp is an experimental SWI-Prolog program for generating constrained melodic phrases and exporting them as MIDI or MusicXML. The directory also contains MuseScore scores, PDFs, and example exports from earlier experiments.

## Files

- `musicomp.pl` defines notes, scales, phrase constraints and generators, and MIDI output. It loads `musicxml.pl`.
- `counterpoint.pl` generates first- through fifth-species counterpoint against a cantus firmus; it is loaded by `musicomp.pl`.
- `musicxml.pl` exports phrases and two-voice scores to MusicXML and constructs timestamped filenames.
- The `.mscz`, `.pdf`, `.mid`, and `.xml` files are existing scores or exports. Their provenance and intended status still need review; the parent repository ignores some of these file types. `.mscbackup/` is ignored.
- [Code review and roadmap](DEVELOPMENT.md) records the review, limitations, and proposed next steps. [GitHub setup](GITHUB.md) explains the parent repository's SSH configuration.

## Getting started

Install SWI-Prolog, then from this directory run:

```sh
swipl -q -l musicomp.pl
```

At the Prolog prompt:

```prolog
?- interval([c,0,4], [g,0,4], I).
I = 7.
?- note_to_midi([c,0,4], M).
M = 60.
?- write_midi([[c,0,4],[d,0,4]], 'example.mid', quarter).
?- write_musicxml([[[c,0,4],[d,0,4]]], 'example.xml').
```

A note is `[Name, Accidental, Octave]`, where `Name` is `c` through `b`, accidental is -1, 0, or 1, and octave is a number. The isometric generators accept `major`, `minor` (natural minor), or `dorian`; counterpoint species currently accept `major` or `minor`. The `gen_phrases` convenience predicates use major and write files in the current directory. For example, `gen_phrases(xml, 10)` writes `combined_output.xml`, overwriting an existing file. Phrase searches can be slow. See the roadmap for remaining limitations.

Run the local tests with:

```sh
swipl -q -s tests/musicomp_tests.pl -g run_tests,halt -t 'halt(1)'
```

Pitch checks use the standard 88-key range A0–C8, including accidentals. Generated intermediate notes use canonical natural or sharp spellings; comparisons and duplicate detection use sounding pitch. The leading-tone approach rule applies in major; natural minor and Dorian have a subtonic and do not apply that rule. In major, an ascending dominant–leading-tone–tonic figure is an allowed exception in any key.

MusicXML export writes one phrase per repeated measure, with whole notes, a treble clef and no time signature. Durations use one division per quarter note. It does not currently infer key signatures or musical spelling from the input scale.

## Randomly choose isometric lines for MusicXML

`choosenisosols(Count, CandidateLimit, Scale, Root, Filename)` collects up to
`CandidateLimit` isometric solutions, randomly chooses `Count` without
replacement, and writes them to a MusicXML file. For example:

```prolog
?- choosenisosols(5, 100, dorian, [d,0,4], 'd_dorian_lines.xml').
```

Use `major`, `minor` (natural minor), or `dorian` and the desired tonic. Both
counts must be positive integers and `Count` must not exceed `CandidateLimit`.
If the search finishes with fewer than `Count` lines, the predicate fails
without opening the file. Otherwise it overwrites the specified file. The
random selection is uniform over the candidates found, which are the first
solutions in Prolog's search order; it is not a uniform sample of all possible
lines. During the search it reports elapsed time and a rough remaining-time
estimate from the average candidate rate. Updates appear as candidates are
found; search difficulty can change, so the estimate can be inaccurate,
especially early on. Finding a large candidate pool may take time.

## Dorian isometric lines

Choose `dorian` and a tonic to generate a modal line; for example,
`isometric_phrase(dorian, [d,0,4], Line)` uses D E F G A B C. The same
isometric constraints apply as in major and minor. Dorian's seventh is a
whole tone below the tonic, so the major leading-tone approach rule is not
applied; no optional raised leading tone is introduced. Generated spellings
use natural notes and sharps, so modes with flats may be written enharmonically.
Dorian is also supported by `diminution/5`; species counterpoint currently
supports major and natural minor only.

## First-species counterpoint

Generate a cantus and an upper counterpoint, then write both voices together:

```prolog
?- once((isometric_phrase(major, [c,0,4], Cantus),
         first_species(major, Cantus, above, Counterpoint))),
   write_counterpoint_midi(Counterpoint, Cantus, 'counterpoint.mid', quarter),
   write_counterpoint_xml(Counterpoint, Cantus, 'counterpoint.xml', piano).
```

The search can take time. Keeping both generators inside `once/1` lets Prolog try another cantus if a given one has no solution. To use a phrase you already have, pass its list as `Cantus`. Use `below` instead of `above` to place the counterpoint below the cantus; then pass `Cantus, Counterpoint` in that order to the writers, which expect upper voice first. `first_species(Scale, Cantus, Counterpoint)` defaults to `above`. Press `;` to find alternatives when not using `once/1`.

`write_counterpoint_midi/3` defaults to whole notes; `/4` accepts `whole`, `half`, or `quarter`. Both voices start together and each note has the same duration. `write_counterpoint_xml/4` accepts `piano` (treble and bass staves), `single` (two voices on one treble staff), or `guitar` (two voices on one octave-below treble staff); `/5` adds a title as its last argument. MusicXML uses whole notes. Both writers require nonempty voices of equal length. Output filenames are relative to the Prolog working directory and existing files are overwritten.

The generator implements a **defined introductory first-species rule set**, rather than every historical school's rules. The cantus must be a valid piano-range major or natural-minor phrase beginning and ending on the same tonic, with a step into the final. The added voice stays on the requested side; vertical intervals are consonant, with a perfect opening and an ending at a unison, octave, or (for upper counterpoint) fifth. There are no interior unisons, voice crossing or overlap, parallel perfect intervals, or similar motion into a perfect interval. A unison or octave cadence approaches the tonic in contrary steps from scale degrees 2 and 7; minor permits the raised seventh at the cadence. An upper fifth ending uses cantus 2–1 against counterpoint 4–5 in contrary stepwise motion. The added voice moves mostly by step, has unique highest and lowest sounding pitches and a range of at most 16 semitones, and recovers from leaps greater than a major third by an opposite step. Larger leaps are restricted. An extreme may occur twice only when it occurs as both the first and last notes, with no interior occurrence. This high- and low-point rule applies to all species. The two voices must have different highest sounding pitches and different lowest sounding pitches. Solutions where their respective high points and low points occur in different measures are returned first; solutions that share an extreme’s measure are returned only after those preferred solutions are exhausted. Tied notes count in every measure where they sound. This ordering can increase search time, particularly when requesting several solutions. Enharmonic intervals are judged by sounding pitch, not notated interval spelling, and generated spellings use sharps. Some valid cantus melodies have no solution under these rules. See [development notes](DEVELOPMENT.md) for limitations and sources.

## Second-species counterpoint

Use `second_species/4` to generate two counterpoint notes for each cantus note, with one final sustained note. For example:

```prolog
?- once((isometric_phrase(major, [c,0,4], Cantus),
         second_species(major, Cantus, above, Counterpoint))),
   write_second_species_midi(Cantus, Counterpoint, 'second.mid'),
   write_second_species_xml(Cantus, Counterpoint, above, 'second.xml', piano).
```

`second_species/3` defaults to `above`; `/4` also accepts `below`. Keep both generators inside `once/1` to let Prolog try a different cantus if needed. The added voice contains `2*N-1` pitches for `N` cantus notes. Its first `2*N-2` pitches are half notes, followed by one whole note; each cantus note is a whole note. MIDI writes synchronized tracks at 480 ticks per quarter note. MusicXML writes one 4/4 measure per cantus note, using either `piano` or `single` layout. For a lower counterpoint, pass `below` to the MusicXML writer as well; the MIDI writer always takes cantus first. The optional sixth argument of `write_second_species_xml/6` specifies a title.

The implementation uses the first-species melodic and cadence rules above, adapted to second species: downbeats are consonant; a weak beat may be dissonant only when approached and left by step in the same direction. Consecutive perfect fifths and octaves on downbeats are excluded, as is similar motion between successive downbeats into a perfect fifth, octave, or unison. Similar-motion approaches to perfect intervals at a cantus change are also excluded. The first note sounds on the downbeat; opening rests and a whole-note penultimate measure are not implemented. Both voices finish on a whole note, with the last counterpoint half note and penultimate cantus note approaching the tonic by contrary step. The same caveats about pitch spelling, historical rule variations, and unsatisfiable cantus phrases apply.

## Third-species counterpoint

Generate four notes against each cantus note with:

```prolog
?- once((isometric_phrase(major, [c,0,4], Cantus),
         third_species(major, Cantus, above, Counterpoint))),
   write_third_species_midi(Cantus, Counterpoint, 'third.mid'),
   write_third_species_xml(Cantus, Counterpoint, above, 'third.xml', piano).
```

Third-species direction choices use the shared contour bias described below, while retaining all the third-species rules.

`third_species/3` defaults to `above`; `/4` also accepts `below`. The added voice has `4*N-3` pitches for `N` cantus notes: four quarter notes per whole-note cantus, then a final whole note in both voices. For a lower counterpoint, also pass `below` to the XML writer. XML layout can be `piano` or `single`; an optional sixth argument supplies the title. Both exports take the cantus first and overwrite existing files.

This is a conservative third-species rule set: beat 1 is consonant. Beat 3 must also be consonant unless beats 2 and 4 are both consonant and beat 3 is approached by step. A dissonance on beats 2, 3, or 4 must also be approached and left by step as a passing or neighboring tone. Consecutive dissonances are excluded, as is repeating the first two quarter notes on beats 3 and 4 of a measure (such as E–F–E–F). Successive four-quarter measures must also have different melodic motion patterns: semitone and whole-tone steps count alike, as do minor and major thirds, with direction preserved. For example, ascending E–F–G–A followed by F–G–A–B is prohibited. Larger leaps are compared by semitone size, and the final whole-note measure is excluded. Pitch comparison uses sounding pitch. The cadence, voice-crossing and perfect-interval restrictions follow the second-species implementation, including the option of an upper final fifth. The counterpoint moves mostly by step, stays within 16 semitones, and follows the same high- and low-point rule as the other species. Opening rests, double neighbors, and the *cambiata* are not implemented. Searches can be slow or fail for some cantus melodies; pitch-spelling limitations also apply.

## Fourth-species counterpoint

Generate a syncopated line with tied suspensions:

```prolog
?- once((isometric_phrase(major, [c,0,4], Cantus),
         fourth_species(major, Cantus, above, Counterpoint))),
   write_fourth_species_midi(Cantus, Counterpoint, 'fourth.mid'),
   write_fourth_species_xml(Cantus, Counterpoint, above, 'fourth.xml', piano).
```

`fourth_species/3` defaults to `above`; `/4` also accepts `below`. Use the same position for the XML writer, which supports `piano` and `single`; an optional sixth argument sets the title. Both writers take the cantus first and overwrite existing files.

`Counterpoint` is a list of measures. The first is `[rest, Note]` (a half rest followed by a half note); each tied interior measure is `[tie(Note), NextNote]`, where `tie(Note)` continues the preceding measure's note through the downbeat. The penultimate measure may instead be `[Strong, Weak]`, two newly attacked half notes with a second-species cadence; the final measure is `[Note]`, a whole note. For example, `[[rest,[c,0,5]],[tie([c,0,5]),[b,0,4]],[[c,0,5]]]` contains a 7–6 suspension over C–D–C. MIDI sustains the tied note without a new attack. MusicXML writes the ties for playback and notation. The high- and low-point rule counts note attacks, not tied continuations.

The line uses continuous syncopation until the penultimate measure; other internal breaks and opening alternatives are not implemented. Weak-beat notes are consonant; held downbeats may be consonant or prepared dissonances. Supported upper suspensions are 4–3, 7–6, and 9–8; lower suspensions are 2–3, 4–5, and 9–10, including octave compounds. Dissonant suspensions resolve downward by step. The penultimate measure may use a 7–6 suspension above or 2–3 below, or break the tie and use two consonant half notes followed by a second-species cadence. The cadence normally approaches the tonic in contrary steps from scale degrees 2 and 7; the shared cadence checker also admits cantus 2–1 against an upper 4–5 ending. The existing motion, range, and unique-extrema rules also apply. This strict profile can have no solution for a given cantus; interval qualities are still assessed by sounding pitch rather than written spelling.

## Fifth-species counterpoint

Generate a mixed-rhythm florid line and export it:

```prolog
?- once((isometric_phrase(major, [c,0,4], Cantus),
         fifth_species(major, Cantus, above, Counterpoint))),
   write_fifth_species_midi(Cantus, Counterpoint, 'fifth.mid'),
   write_fifth_species_xml(Cantus, Counterpoint, above, 'fifth.xml', piano).
```

`fifth_species/3` defaults to `above`; `/4` also accepts `below`. Pass the same position to the XML writer and choose `piano` or `single`. An optional sixth XML-writer argument sets the title. The writers take the cantus first, and overwrite existing files.

The output is a list of measures containing explicit events: `rest(half)` and `note(Pitch, Duration, Tie)`, where duration is `quarter`, `half`, or `whole` and tie is `start`, `stop`, or `none`. Each measure lasts one cantus whole note. Tie continuations are sustained rather than reattacked; both voices end with a whole note.

This first florid profile is intentionally narrower than the full fifth-species tradition. It starts from a valid fourth-species line and divides one or more tied half notes into a tied quarter followed by a consonant quarter note, producing a mix of quarter and half notes with suspensions. The line begins with a half rest and retains continuous ties, a suspended fourth-species cadence, existing range, unique-extrema and motion rules. It does **not** yet generate eighth notes, dotted rhythms, free combinations of all earlier species, opening alternatives, or other florid figures. In particular, it does not add passing or neighboring dissonances in the inserted quarter notes. The cantus must meet the fourth-species cadence requirements, and some valid cantus lines have no solution in this profile. See [development notes](DEVELOPMENT.md) for planned extensions and references.

## Several solutions in one MusicXML score

Use `findnsols/4` to collect solutions for one fixed cantus, then pass the resulting list to `write_solutions_musicxml/6`:

```prolog
?- Cantus = [[c,0,4],[b,0,3],[c,0,4],[d,0,4],
             [c,0,4],[e,0,4],[d,0,4],[c,0,4]],
   findnsols(5, Counterpoint,
             third_species(major, Cantus, above, Counterpoint),
             Solutions),
   write_solutions_musicxml(Cantus, third, above, Solutions,
                            'solutions.xml', piano).
```

The species argument is `first`, `second`, `third`, `fourth`, or `fifth`; it must match the generator used to produce `Solutions`. Set position to `above` or `below`, matching the generator, and layout to `piano`, `single`, or `guitar`. The optional seventh argument supplies a score title. Solutions appear sequentially, each labeled and accompanied by the same cantus; double barlines separate them. The combined MusicXML requests a system break after every eight measures across the full score, in either staff layout. A notation program may still reflow the score or insert earlier breaks if the measures do not fit. Clefs and time signatures are declared once at the beginning rather than repeated for each solution. Rhythms and ties follow the corresponding single-solution writers.

`findnsols/4` may find fewer results than requested and can take time. The writer requires at least one solution, validates the shape and ties of each input, and overwrites the destination. It does not independently verify every counterpoint rule or infer the species or position. The combined writer produces MusicXML; there is no combined MIDI writer yet.

An upper fifth is permitted by the shared ending and cadence predicates, and can be generated in first and third species. Other existing constraints still apply. In particular, the second- and fourth-species profiles' rules against repeated counterpoint notes and hidden or parallel perfect intervals can prevent fifth endings; the current fifth-species profile also requires a suspended tonic cadence. The cantus continues to end on its tonic. These limitations mean that requesting more solutions does not guarantee a fifth ending for every species or cantus.

## Melodic contour and direction bias

All five species choose `up` or `down` with equal probability at the start of a randomized search. For a multi-solution query, each species makes a fresh choice on each of up to 40 randomized attempts and suppresses duplicate melodies before continuing with the complete search. It also chooses a target measure uniformly from the first through the penultimate measure. For targets after the first, up to and including the target it tries the chosen direction first with 60% probability at each eligible interior note; afterward, it biases motion in the opposite direction to encourage a return toward the cadence. When the target is the first measure, it instead biases subsequent eligible choices downward, for either curve. During randomized trials it seeks a solution whose high point (for `up`) or low point (for `down`) sounds in the target measure. For example, `up` encourages an arch and `down` encourages a valley. The bias affects candidate order, not a mandatory shape or the probability distribution of the resulting solutions. Existing constraints, forced suspension resolutions, tied notes, and cadences take precedence; backtracking can use the other direction.

To choose a different probability and see the selected curve, use `species_with_contour/7`:

```prolog
?- Cantus = [[c,0,4],[d,0,4],[c,0,4]],
   once(species_with_contour(first, major, Cantus, above,
                             0.75, Curve, Counterpoint)).
```

The arguments are species (`first` through `fifth`), scale, cantus, position, bias, curve, and result. To inspect or specify the target measure, use the eight-argument form, with the one-based target immediately before the result:

```prolog
?- Cantus = [[c,0,4],[b,0,3],[c,0,4],[d,0,4],
             [c,0,4],[e,0,4],[d,0,4],[c,0,4]],
   once(species_with_contour(first, major, Cantus, above,
                             0.6, up, 4, Counterpoint)).
```

Leave the target unbound to see the randomly selected measure; specify an integer from 1 through the penultimate measure to choose it. If no target-matching result is found within the bounded random trials, the ordered fallback may have its extreme elsewhere. Bias is a number between `0.0` and `1.0`: `0.75` tries the preferred direction first 75% of the time. Leave `Curve` unbound for a 50–50 random choice, or supply `up` or `down` to select it. The usual species predicates use `0.6` and choose the curve randomly. Call `set_random(seed(42))` before the query for reproducible results.

To avoid very slow random search paths, randomized trials have a fixed search budget. If they do not find a solution, the generator uses the complete ordered search. For all species, further solutions on backtracking first come from additional independent randomized attempts, with duplicates removed; after those attempts, the complete fallback search supplies remaining solutions while drawing among opening intervals. Direct `species_with_contour` calls use the complete fallback after their first randomized result. The returned curve and target identify the preference attempted, not a guarantee that a fallback or later backtracking solution reaches that extreme in the chosen measure. A 50–50 choice of contour is not a guarantee that half the returned melodies will have each shape: constraints, unsuccessful attempts, and duplicate filtering affect the result. In particular, if a minimum occurs in the first measure, subsequent notes cannot descend below it while it remains the overall minimum, so the downward bias must give way to other choices or the fallback. Tied notes count in every measure where they sound. The high- and low-point preference between the voices remains in force.

## Guitar notation

All two-voice and multi-solution MusicXML writers accept `guitar` as the layout. It places both voices on one treble staff with an 8 below the clef. For guitar layout, an input such as G4 is displayed at the written G4 position and sounds as G3 in MuseScore. The XML encodes the sounding pitch one octave below the input and uses the octave-below clef without a second transposition. This is an octave lower than a MIDI file written directly from the same input. The option does not check a guitar's physical range, fingering, or whether both voices can be played simultaneously. Inputs in octave 0 cannot be represented one octave lower in MusicXML and are rejected for this layout.

To try it with your cantus:

```prolog
?- Cantus = [[c,0,4],[b,0,3],[c,0,4],[d,0,4],
             [c,0,4],[e,0,4],[d,0,4],[c,0,4]],
   findnsols(3, Counterpoint,
             third_species(major, Cantus, above, Counterpoint), Solutions),
   write_solutions_musicxml(Cantus, third, above, Solutions,
                            'guitar_test.xml', guitar).
```

Open `guitar_test.xml` in MuseScore and check the 8 below the treble clef, both voices, and playback. The same layout option works with `write_counterpoint_xml/4` and the single-solution `write_second_species_xml/5` through `write_fifth_species_xml/5` predicates.

## Random starting interval

For every species, the first sounding counterpoint note is selected relative to the opening cantus note. Each randomized solution attempt draws a unison with 30% probability, a perfect fifth with 30%, or an octave with 40%; this applies above or below the cantus and includes fourth and fifth species after their initial rest. A multi-solution search makes fresh weighted draws rather than fixing one opening for the whole batch.

These are probabilities for the **draw**, not guarantees about a batch's final proportions. Range limits, the other counterpoint rules, contour preferences, duplicate removal, and finite randomized search can prevent a drawn opening from producing a result. The generator can backtrack to another opening. Its complete fallback draws among opening-specific searches using the same weights, adjusting for searches that have been exhausted. Set `set_random(seed(42))` before a query to reproduce its random choices.

## Diminution of an isometric melody

`diminution(Scale, Root, Phrase, Mode, Measures)` generates a monophonic
variation of a line of whole-note pitches. `Scale` is `major`, `minor` (natural minor), or `dorian`,
`Root` and each pitch have the usual `[letter, accidental, octave]` form, and
`Mode` is `halves`, `quarters`, `syncopated`, or `mixed`. Each original pitch
sounds at the beginning of its corresponding measure, and the final pitch
fills a whole measure. The final sounding pitch is approached from the
previous attack by a semitone or whole tone; a tied continuation is not a
new attack. Added pitches stay within the original pitch range.
Backtracking yields alternative variations.

For example, in `swipl -q -l musicomp.pl`:

```prolog
?- Line = [[c,0,4],[b,0,3],[c,0,4],[d,0,4],
           [c,0,4],[e,0,4],[d,0,4],[c,0,4]],
   once(diminution(major, [c,0,4], Line, quarters, Variation)),
   write_diminution_xml(Variation, 'diminution.xml', guitar),
   write_diminution_midi(Variation, 'diminution.mid').
```

Use `single` in place of `guitar` for an ordinary treble clef. As with the
other guitar writers, the guitar score is encoded to sound an octave below the
internal notes in MuseScore; the MIDI writer uses the internal pitches.
The optional fourth argument of `write_diminution_xml/4` supplies a title.
The result is a list of measures containing events such as
`note([c,0,4],half,none)`; durations are `whole`, `half`, `quarter`, and
`eighth`, and ties are `none`, `start`, or `stop`.

The implementation follows the photographed instructions conservatively:
for half-note, quarter-note and mixed variations, an immediately repeated
two-note motif is rejected, including across bar lines; adjacent four-quarter measures cannot repeat the same directional
interval pattern, and an identical four-quarter measure cannot recur later.
Quarter-note downbeats cannot leap upward, and leaps across bar lines require
contrary stepwise continuation. It favors stepwise motion and avoids two
successive thirds in the same direction (a triad outline). Syncopated
variations anticipate selected anchors with a tied half note and also include
ordinary half notes. Mixed variations combine half and quarter measures,
including half–quarter–quarter and half–quarter–eighth–eighth measures;
eighth notes occur only in pairs following a quarter, in at most one measure. Mixed mode also permits tied half notes alongside quarters. The generator
does not implement every possible changing-note or syncopation figure shown
in the source. Preserving anchors and pitch range helps retain the original profile,
but does not guarantee every aspect of a melody's perceived contour.

Run the focused tests with:

```sh
swipl -q -s tests/diminution_tests.pl -g run_tests,halt -t 'halt(1)'
```

To put several diminution variations in a single MusicXML score:

```prolog
?- Line = [[c,0,4],[b,0,3],[c,0,4],[d,0,4],
           [c,0,4],[e,0,4],[d,0,4],[c,0,4]],
   findnsols(5, Variation,
             diminution(major, [c,0,4], Line, quarters, Variation),
             Variations),
   write_diminution_solutions_xml(Variations, 'quarter_solutions.xml', guitar).
```

The writer accepts `single` or `guitar`, labels the variations, separates them
with double barlines, and requests a system break after every eight measures.
It validates all variations before opening the output file. An empty list or
an invalid variation fails; a valid call overwrites an existing file. Use
`write_diminution_solutions_xml(Variations, Filename, Layout, Title)` to set a
custom title. MuseScore may insert additional system breaks to fit the music.
