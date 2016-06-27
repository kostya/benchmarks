Brainfuck interepter and benchmark
----------------------------------

In benchmark i using only standard language containers and compile keys, without any kind of hacks to fair compare.
All implementations using Tape as standard language array, starting from size 1. It interepter all bf instructions, one by one, without any squash or other hacks. Stdout should be flushed after each symbol.

To compile all: `sh build.sh`

To run all: `sh run.sh`
