# harmony-generator
Prolog Code to generate modulating chord sequences

## Rationale
Rather than a single monolithic application, this is a set of modules constituting a harmony library.
Each module is intended to take the output of a previous module and rewrite it in some way, in a pipeline.
The intention is to be able to pick and mix later modules, relying on a common intermediate format but at the
moment each module is more or less tied to the format of the previous module's output.

## Directory Layout

* `src/run.pl` - top level pipeline.
* `src/original.pl` - an early attempt, all code in one file, kept for reference.
* `src/harmony/` - the library
