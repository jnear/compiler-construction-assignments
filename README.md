# UVM Compiler Construction Assignment Code

## Assignment instructions

For your homework exercises, you will be expected to implement various
compiler passes. It will ultimately be up to you how exactly to do
this, but for the second assignment you are given code templates in
`compiler-A2.rkt` to fill out.

To start out, you must "clone" this repository:

```
   git clone https://github.com/jnear/compiler-construction-assignments.git
```

This creates a new folder `compiler-construction-assignments`, which you can enter with:

```
   cd compiler-construction-assignments
```

Before each assignment (and when told to by the instructor), you may need to update
this code by pulling updates from GitHub by running this command from inside the folder:

```
   git pull
```

As you fill out the functions in `compiler-A2.rkt`, tests are run with the
`run-tests-A2.rkt` module. You can run these tests either from the command
line with:

```
   racket run-tests-A2.rkt
```

Or by opening and running `run-tests-A2.rkt` in DrRacket.

Before running the compiler tests, you need to compile
`runtime.c` (see below).

## Assignment Submission

Submit your assignment on Blackboard. First, make sure that `racket
run-tests-A2.rkt` produces the correct output (i.e. all the tests
run). Then, zip up your whole `compiler-construction-assignments`
directory into a ZIP file:

```
   cd ..
   zip a2.zip compiler-construction-assignments
```

Submit this ZIP file on Blackboard as your assignment solution.

## Public student code

Utility code, test suites, etc. for the compiler course.

This code will be described in the Appendix of the book.

The `runtime.c` file needs to be compiled and linked with the assembly
code that your compiler produces. To compile `runtime.c`, do the
following
```
   gcc -c -g -std=c99 runtime.c
```
This will produce a file named `runtime.o`. The -g flag is to tell the
compiler to produce debug information that you may need to use
the gdb (or lldb) debugger.

Next, suppose your compiler has translated the Racket program in file
`foo.rkt` into the x86 assembly program in file `foo.s` (The .s filename
extension is the standard one for assembly programs.) To produce
an executable program, you can then do
```
  gcc -g runtime.o foo.s
```
which will produce the executable program named a.out.
