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
    * `6502asm.fth` - the 6502 assembler, needed to compile `Code` words
    * `trns6502asm.fth` - the transient 6502 assembler. It lives on the
    heap and is removed by `clear`. This allows building applications that
    have code words but don't carry the assembler itself after saving.
    * `tmp6502asm.fth` - like the transient 6502 assembler, but living on the
    tmpheap instead of the heap. See below for tmpheap.
    * `rom-ram-c16.fth` - macros for C16 bank switching
    * `rom-ram-c64.fth` - macros for C64 bank switching
    * `tracer.fth` - the debugger
    * `tasker.fth` - the multitasker
    * `multitask.fth` - the small bit of assembly code needed by tasker.fth
    * `taskdemo.fth` - a C64/C16 demo of the tasker
    * `taskdemo-x16.fth` - a X16 demo of the tasker
    * `x16input-tsk.fth`- The usual v4th X16 keyboard input uses the regular
    screen editor for line input, so no task switches happen during line input.
    This input implementation allows task switches during input but uses Kernal
    variables and has a cursor bug after backspace.
    * `cbmopen.fth` - Forth words for Kernal channel I/O
    * `lists.fth` - two list utility words
    * `profiler.fth` - *** [4d2023-04](https://forth-ev.de/wiki/res/lib/exe/fetch.php/vd-archiv:4d2023-04.pdf) (English)
    * `tmpheap.fth` - the reference implementation of the tmpheap design
    as described at
    [SVFIG 04-2021](https://www.forth.org/svfig/kk/04-2021.html), in the
    [4d2021-03](https://forth-ev.de/wiki/res/lib/exe/fetch.php/vd-archiv:4d2021-03.pdf) (German) and used
    in [cc64](https://github.com/pzembrod/cc64/blob/master/src/cc64/cc64.fth#L11)
    * `x16tmpheap.fth` - a X16 tmpheap implementation that uses banked ram
    * `notmpheap.fth` - a null implementation that redirects the tmpheap
    to the regular VolksForth heap
    * `tc-base.fth` - loadfile for the resident part of the target compiler

* `src_petscii/` - the files from `src/` converted to PETSCII

* `tests/` - the test files used to validate VolksForth builds

* `tests_petscii/` - the files from `tests/` converted to PETSCII

## Versions

The following version descriptions are only valid for VolksForth 6502
C64 Releases. As of now (June 2024), the different VolksForth platforms
(6502, 68k, 8080, 8086) don't have shared code or shared versioning.

### 6502-C64 3.9.6 - in preparation

Several dependencies of the VolksForth kernel on X16 Kernal variable addresses
and thus likely breakage points with new Kernal release were replaced with
Kernal API calls, so the VolksForth kernel should now be much more robust
when the X16 Kernal changes. The only remaining Kernal variable used is
$0376 - BkgPenCol.

* Clearing the IOStatus is now possible through the ExtApi call ($FEAB, thanks
  to *** for implementing this), so the dependency on the address of IOStatus
  could be removed.
* The implementation of line input in EXPECT was changed from switching cursor
  on and off and using GETIN to using BASIN - which automatically handles the
  cursor and also makes use of the CBM screen editor. The Kernal var dependency
  that came with switching the cursor on and off is now removed.
  The downside is that the
  cooperative multitasker now can't run tasks during line input. The old
  tasker-compatible EXPECT is now available as separtate source
  x16input-tsk.fth. For details on this see MISC_DOC.md.
* The direct clearing of MsgFlg (X16: $028d) at the beginning of
  (busin and (busout was removed; it shouldn't have had any real purpose.
* Likewise the clearing of the Kernal variables QtSw and Insrt after each
  char written to the console via CHROUT: It was removed from the X16 variant
  as it shouldn't be necessary.

The cooperative multitasker was extracted from the original disk 3 (see
[`disks/vforth4_3.fth`](https://github.com/forth-ev/VolksForth/blob/master/6502/C64/disks/vforth4_3.fth))
into the files `tasker.fth, multitask.fth` and `taskdemo.fth`. The latter was
ported to the X16 as `taskdemo-x16.fth`.

`rom-ram-sys.fth` was split up into a C64 and a C16 flavour. There is no X16
equivalent; X16 bank switching poses challenges and opportunities completely
different from those on C16 and C64.

New sources added: `lists,fth, tasker.fth, multitask.fth,
taskdemo.fth, taskdemo-x16.fth, x16input-tsk.fth, tmp6502asm.fth,
x16tmpheap.fth, rom-ram-c16.fth, rom-ram-c64.fth`

### 6502-C64 3.9.5

This release adapts the X16 VolksForth to the R46 ROM.
It also adds an X16 binary with added words to invoke the
ROM-based X16Edit (XED), to list directories and files (DIR and CAT)
and to issue DOS commands and read the error channel (DOS).

For C64 and C16 this release should be a no-op.
