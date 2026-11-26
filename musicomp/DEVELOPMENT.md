# Code review and development roadmap

This review covers both Prolog files in this directory. The project is a useful prototype, but its generators and exporters need tests and validation before their output should be treated as authoritative.

## How the code works

`musicomp.pl` represents notes as `[name, accidental, octave]`; `note_value/2` converts them to semitone numbers and `interval/3` computes absolute distances. Major, natural minor and Dorian scales are defined by pitch-class offsets. `isometric_phrase/3` generates 8–12-note phrases with a fixed root at each end; `isometric_phrase_constrained/5` also fixes the second and penultimate notes. Constraints cover range, a unique high point, final step, leaps, sixths, sevenths, tritones, broken triads, repeated patterns, and the leading tone. The history variants use a non-backtrackable global list. MIDI functions produce a single-track format-0 file with 480 ticks per quarter note and a uniform duration. Convenience `gen_phrases` predicates collect solutions and write MIDI or XML.

`musicxml.pl` exports a single-part score, with each phrase in one repeated measure and each note written as a whole note. It provides default or supplied titles and a timestamp-based filename helper. Existing score files provide examples, though their relationship to the current code has not been established.

## Review findings and next steps

Implemented: notes are checked by sounding pitch against A0–C8, enharmonic equivalents share a canonical natural/sharp spelling, generated candidates are deduplicated, and repeated pitches and patterns compare consistently. Phrase lengths are bounded to 8–12. The leading-tone exception now transposes with the major key; natural minor does not apply this major leading-tone rule. The XML writer escapes text, closes its stream on exceptions, and declares divisions, an unmetered time indication, and treble clef.

1. **Expand the tests.** `tests/musicomp_tests.pl` covers pitch boundaries, enharmonics, rules, a generated phrase, MIDI, and XML parsing. Add wider property and regression coverage for all constraints, history and error handling. The parent repository's test scripts are separate from this directory.
2. **Improve generation and history.** The code constructs complete candidate phrases before most checks, producing a large search space. Apply constraints incrementally and benchmark representative requests. Global `generated_phrases` persists between queries, is initialized on load, and is changed even on backtracking; document or replace it with explicit state and a reset operation. `findnsols/4` can return fewer results than requested when solutions are exhausted.
3. **Develop notation and output.** MusicXML currently uses whole notes, one phrase per repeated measure, a treble clef and no time signature or inferred key signature. Check real scores in notation software, decide on appropriate rhythm, key-aware enharmonic spelling and layout, and add fixtures. MIDI supports only three durations and fixed VLQ values; input pitches are not checked against MIDI's 0–127 range. Convenience output names can overwrite existing files. Timestamp filenames are not guaranteed unique and titles are not sanitized beyond spaces.
4. **Organize the API and artifacts.** Consider making `musicomp.pl` a module with explicit exports, separating musical rules, generation, and serialization, and documenting predicate modes and error behavior. Decide which existing scores and exports are source material, examples, or generated output before changing their tracking. Select a license before publishing if reuse is intended.

## Validation

Run `swipl -q -s tests/musicomp_tests.pl -g run_tests,halt -t 'halt(1)'`. A representative exported score with escaped title and subtitle was additionally checked against the W3C MusicXML 3.1 and 4.0 XSDs using `xmllint`; schema validation confirms structure, not engraving quality or musical intent. The writer labels its output MusicXML 3.1.

## First-species scope

`counterpoint.pl` supplies an incremental, finite first-species search above or below a supplied cantus. It implements a deliberately specific teaching rule set; see the README for the exact constraints. Approaches differ between schools. The current pitch-class model cannot distinguish enharmonically equivalent interval spellings, checks melodic shape through a few numerical constraints rather than all stylistic guidelines, and does not enforce every preference (such as climax placement or limits on consecutive imperfect consonances). It is not a complete historical counterpoint validator. The cantus itself is checked for range, scale, tonic endpoints, and cadence, not for all melodic rules.

The rule choices were informed by the [University of Victoria counterpoint notes](https://www.uvic.ca/finearts/music/assets/docs/Counterpoint%20online.pdf) and [Open Music Theory's first-species chapter](https://human.libretexts.org/Bookshelves/Music/Music_Theory/Open_Music_Theory_2e_%28Gotham_et_al.%29/02%3A_Counterpoint_and_Galant_Schemas/2.02%3A_First-Species_Counterpoint). Further work could add key-aware diatonic spelling, alternative rule profiles, configurable voice ranges and scoring, and tests for additional modes and cadences. The two-voice MusicXML uses unmetered whole notes in a single measure; MIDI supports uniform whole, half, and quarter notes. Check the engraving in the intended notation program.

## Second-species scope

`second_species/4` builds two notes against each cantus note, ending with one longer note in each voice. Its weak-beat dissonances are strictly passing tones; tests cover the passing rule, parallel and hidden downbeat perfect intervals, MIDI timing, and MusicXML measure rhythm. It follows a deliberately conservative profile and does not implement optional opening rests, all historical melodic conventions, or a whole-note penultimate measure. It uses the same sounding-pitch comparison and range/shape restrictions as first species. The [University of California, Santa Barbara second-species notes](https://rothfarb.faculty.music.ucsb.edu/courses/103/Second_Species%282v%29.html) and [Open Music Theory](https://openmusictheory.github.io/secondSpecies.html) describe the underlying approaches. Future development could allow alternative rule profiles, more cadence patterns and rhythm representations, and test notation in a score editor.

## Third-species scope

`third_species/4` generates four quarter notes against a whole-note cantus, with a shared whole-note ending. A two-note quarter-note motif cannot repeat immediately within the same measure. Successive four-quarter measures cannot repeat the same three-movement pattern: half and whole steps are equivalent, minor and major thirds are equivalent, and larger leaps are compared by semitone size; direction is retained. The final whole-note measure is excluded. Beat 3 must be consonant unless beats 2 and 4 are consonant and it is approached by step; the existing dissonance checks also require it to resolve as a passing or neighbor tone. Non-downbeat dissonance is limited to isolated passing or neighbor tones; double neighbors, cambiata, and optional opening rests are not implemented. It applies the second-species downbeat, motion and cadence checks, plus mostly-stepwise motion, a 16-semitone range, and unique highest and lowest pitches (allowing the same extreme at both endpoints only). Its search space grows quickly and some cantus phrases may take substantial time or yield no solution. The [UCSB third-species notes](https://rothfarb.faculty.music.ucsb.edu/courses/103/Third_Species%282v%29.html) and [Open Music Theory](https://openmusictheory.github.io/thirdSpecies.html) explain additional traditional figures that a later rule profile could support. MIDI and MusicXML preserve the four-against-one rhythm and final whole note; the schema checks do not replace testing in notation software.

All species enforce unique highest and lowest sounding pitches in the counterpoint. Either extreme may occur at both the first and last notes, provided it does not occur anywhere else. This additional constraint can reduce the number of solutions or increase search time.

## Fourth-species scope

`fourth_species/4` constructs a strict syncopated line: a half rest, preparations tied across barlines, descending stepwise resolutions of permitted dissonances, an untied penultimate resolution, and a final whole note. It implements upper 4–3, 7–6 and 9–8, and lower 2–3, 4–5 and 9–10 suspensions (including octave compounds). The penultimate measure may instead break the tie and use two consonant half notes and a second-species cadence. Earlier breaks, alternate beginnings, and all historical spellings are not implemented. Compare the [UCSB fourth-species notes](https://rothfarb.faculty.music.ucsb.edu/courses/103/Fourth_Species%282v%29.html) and [Open Music Theory](https://openmusictheory.github.io/fourthSpecies.html). MusicXML uses both sound and notation tie elements; MIDI gives tied notes one continuous duration. Schema and timing tests do not substitute for checking playback and engraving in the intended applications.

## Fifth-species scope

`fifth_species/4` is a deliberately limited florid profile, not a full implementation of every fifth-species figure. It derives a fourth-species skeleton and replaces one or more tied half-note continuations with a tied quarter note and a consonant quarter-note attack. The remaining half-note preparations preserve ties and suspensions. It checks the resulting attacks for melodic movement, range, unique extremes, and interval restrictions. A suspended fourth-species cadence, starting rest, and continuous ties remain mandatory for fifth species. Future work could generate free combinations of species and measure patterns, including weak-beat dissonant passing and neighbor tones, optional breaks in syncopation, eighth-note pairs, dotted rhythms, and further ornamental suspension resolutions. See [Open Music Theory's fifth-species chapter](https://viva.pressbooks.pub/openmusictheorycopy/chapter/fifth-species-counterpoint/) and [Ars Nova's guide](https://www.ars-nova.com/cpmanual/fifthspecies.htm). MIDI encodes tied quarter continuations as a single 1440-tick sounding note; MusicXML writes both sound and notation ties. Validate playback and engraving in the target application.

## Multi-solution scores

`write_solutions_musicxml/6` and `/7` assemble labeled, sequential solutions for one cantus and species. They reuse the single-solution serializers, validate all section shapes before opening the destination, renumber measures, preserve ties and durations, and request a system break after every eight measures across the combined score. The writer checks structure, not musical correctness; callers supply a list and its matching species and position. Empty solution lists fail. Tests cover all species; combined examples have also been checked against the MusicXML 3.1 schema. A combined MIDI writer is not implemented.

The generators also reject pairs whose voices have the same highest sounding pitch or the same lowest sounding pitch. For each species, solutions with the two voices' high points in different measures and low points in different measures are enumerated before fallback solutions sharing either extreme's measure. A held note counts in every measure in which it sounds. The preference does not discard fallback solutions, but exhausting the preferred search can take longer before fallback results appear. Fifth species ranks its completed florid line, not its intermediate fourth-species skeleton.

All species select an equally likely up or down contour for each search, and a target measure from the first through the penultimate, then bias eligible interior direction choices toward the chosen extreme through that measure and in the opposite direction afterward. When the first measure is targeted, subsequent choices are biased downward for either curve; a first-measure minimum necessarily limits that downward motion. Randomized trials seek solutions with the chosen high or low point sounding in the target measure, with tied notes counted in each measure. `species_with_contour/7` permits a caller-supplied probability and optionally a fixed contour; `/8` also exposes or selects the target measure. Random trials are bounded. All species make up to 40 independent randomized attempts with fresh contour and opening choices and deduplicate results before continuing with a complete weighted fallback search. The bias affects search order and is not a guarantee about final shape or uniform sampling; the ordered fallback may not meet the target. The scoped, thread-local non-backtrackable context is restored on completion, failure, or exception. Third species maintains its pitch range incrementally and, during randomized target searches, prunes prefixes that have already made the target extreme impossible. Direction filtering occurs before evaluating candidate pitches. The complete ordered fallback does not apply target pruning. These optimizations retain the existing constraints; random seeds can produce different solutions from earlier versions.

Upper counterpoint may end a perfect fifth above the cantus, with a contrary-step cadence from cantus scale degree 2 and counterpoint degree 4 to degrees 1 and 5. Lower endings remain unison or octave. First- and third-species examples are covered by tests. The existing second- and fourth-species restrictions on repeated notes and downbeat perfect intervals can still preclude this cadence; the current fifth-species profile requires a suspended tonic cadence. Supporting a fifth in those profiles would require an explicit decision about which existing rule or rhythmic pattern to relax.

Guitar MusicXML uses one staff with two voices and an octave-below G clef. It encodes notes an octave below the input, with no additional transpose element; MuseScore displays the input pitch and sounds it an octave lower. MuseScore command-line MIDI conversion was checked against known pitches, and the tests exercise standalone and combined writers for all species, tied notes, and the MusicXML octave limit. Guitar range and fingering are not validated.

All species draw the counterpoint's opening interval with weights 30% unison, 30% perfect fifth and 40% octave, including for lower counterpoint and after an opening rest. Each independent randomized sample gets a fresh draw. If the preferred opening cannot yield a valid line, backtracking can use another. The complete fallback merges opening-specific solution streams using weighted draws and removes exhausted streams; cooperative yielding prevents a difficult stream from monopolizing the search. Outcome frequencies need not match draw weights because of constraints, search limits, and deduplication.

## Isometric lines: comparison with photographed specification

The three pages `isometric1.png`–`isometric3.png` specify an isometric line
with equal note values, major, Aeolian minor or melodic minor, tonic at both
ends at the same pitch, a step into the final, a single high point, and a
range of at most an octave. The generators now enforce that octave limit;
`range_within/1` remains the separate 16-semitone limit used for counterpoint.
Both isometric generators enforce tonic endpoints, stepwise final approach,
unique maximum, no repeated adjacent pitches, no tritones or sevenths, the
allowed direction of sixths, and contrary recovery after leaps larger than a third. Recovery is normally
stepwise; after a perfect fourth, an opposite third is also allowed. The book
allows non-step contrary motion after leaps to structural tones more broadly,
but the program does not yet identify those tones.

Differences still requiring musical decisions or additional implementation:

- `minor` is natural minor only; `dorian` is also available as an additional
  mode. Melodic minor and its direction-dependent
  sixth and seventh degrees are not supported. Harmonic minor is not generated.
- The book prefers seconds and uses thirds less frequently. The isometric
  generators have no explicit overall step/third preference or scoring rule.
- Repetition of an adjacent two-note group is forbidden, but longer repeated
  groups and repeated transposed sequences of leaps are not generally detected.
- Broken triads are detected when three consecutive pitches form a major,
  minor, diminished, or augmented triad, in any order or octave. They are
  compared by sounding pitch, rather than key-aware spelling.
- Leading tones in major must be approached stepwise except for one cadence
  figure; the book calls stepwise approach a preference. Natural minor has no
  leading-tone check. Structural tones are not identified, so the more
  permissive structural-tone leap rule cannot currently be applied reliably.
- Sounding semitones and canonical sharp spellings stand in for spelled
  intervals, so augmented or diminished intervals cannot all be distinguished
  from enharmonically equivalent permitted intervals. No key-aware spelling
  or general chromaticism analysis is performed.
- Phrase lengths of 8–12 are a program choice, not a rule stated on these
  pages. The illustrated task also asks the student to analyze the melodic
  profile; there is no dedicated isometric profile-analysis predicate.
