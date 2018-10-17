# harmony-generator
Prolog Code to generate modulating chord sequences

## Rationale
Rather than a single monolithic application, this is a set of modules constituting a harmony library.
Each module is intended to take the output of a previous module and rewrite it in some way, in a pipeline.
The intention is to be able to pick and mix later modules, relying on a common intermediate format but at the
moment each module is more or less tied to the format of the previous module's output.

The plan is to add modules that will do voice placement (SATB ranges,) voice leading (inversions, common notes,
suspensions, anticipations and passing tones,) validation (disallow parallel fifths etc.) and finally output
to MusicXML that can be directly imported into notation software like Sibelius or MuseScore.

## Directory Layout
* `src/run.pl` - top level loader.
* `src/original.pl` - an early attempt, all code in one file, kept for reference.
* `src/harmony/` - the library.
* `src/harmony/chords.pl` - utility, calculates notes of chord.
* `src/harmony/key_chain.pl` - pipeline, generates a sequence of keys.
* `src/harmony/note.pl` - utility, map note names to numbers.
* `src/harmony/padding.pl` - pipeline, takes sequence of pivot chords and adds padding chords.
* `src/harmony/pivot_chords.pl` - pipeline, takes sequence of keys and produces sequence of pivot chords.
* `src/harmony/sequences.pl` - utility, common chord sequences for padding.
