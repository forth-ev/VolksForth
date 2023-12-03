# VolksForth 6502 C64 Releases

Release notes of VolksForth for 6502 on CBM-like machines (C64/C16/X16)

## Release content

The latest release zip file `volksforth-6502-c64-release.zip` contains

* `binaries/` - the compiled VolksForth binaries, namely:
    * `v4th-c64` - minimal C64 kernel
    * `v4thblk-c64` - C64 kernel with added block words
    * `v4th-c16-` - minmal C16 kernel using 32k RAM
    * `v4thblk-c16-` - C16 kernel using 32k RAM with added block words
    * `v4th-c16+` - minmal C16 kernel using 63k RAM
      (with bank switching)
    * `v4thblk-c16+` - C16 kernel using 63k RAM with added block words
    * `v4th-x16` - minmal Commander X16 kernel
    * `v4th-x16e` - Commander X16 kernel with added X16Edit and
      DOS commands

* `src/` - sources
    * `v4th*.fth` - the binaries' main files
    * `vf-*.fth` - sources from which VolksForth
      kernels are compiled
    * further Forth sources

* `src_petscii/` - the files from `src/` converted to PETSCII

* `tests/` - the test files used to validate VolksForth builds

* `tests_petscii/` - the files from `tests/` converted to PETSCII

## Versions
The following version descriptions areonly valid for VolksForth 6502
C64 Releases. As of now (Dec 2023), the different VolksForth platforms
(6502, 68k, 8080, 8086) don't have shared code or shared versioning.

### 6502-C64 3.9.5

This release adapts the X16 VolksForth to the R46 ROM.
It also adds an X16 binary with added words to invoke the
ROM-based X16Edit (XED), to list directories and files (DIR and CAT)
and to issue DOS commands and read the error channel (DOS).

For C64 and C16 this release should be a no-op.
