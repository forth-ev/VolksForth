
# The Multitasker

VolksForth comes with a simple but powerful multitasker which allows the
creation of printer spoolers, clocks, counters and other simple background
tasks.

The main characteristic of this multitasker is that it uses cooperative
multitasking instad of preemtive multitasking.
This means that each task has to explicitly yield
execution control and access to I/O devices to make them available for other
tasks. However, each task can choose when this happens, esp. when to yield
execution control. Of course this must happen often enough for all tasks
to work well. Execution control is yielded via the word PAUSE.

## Motivating Use Case

Block 74 of [Disk 3](../disks/vforth4_3.fth) of the original
VolksForth/UltraForth distribution contains a print spooler that creates a task
`Printspool` to print a range of source blocks via `pthru` in the background
while the user can still interact with the system in the foreground.

Caveat: The printer code hasn't yet been extracted into `.fth` files and may
need some adaption to current VolksForth versions and esp. to the X16.
In particular, source block printing doesn't make much sense with the X16 as of
now because block source compiling is currently only supported on the C64 and
C16/Plus4.

## Task Demo

A simpler task demo has been extracted from Disk 3's block 62 into
[src/taskdemo.fth](../src/taskdemo.fth).

### On C64 or C16/Plus4
```
include tasker.fth
include taskdemo.fth
```
will create a counter in the top left corner, counting down in the background
from 256 to 0. A fun way to see the time spent in the background task is to
type
```
words
```
while the counter is running: The speed of words listing will increase
noticably once the counter has run out.

### On the X16

On the X16 things are slightly different:
```
include tasker.fth
include taskdemo-x16.fth
```
will create a counter in the top left corner, counting down in the background
from 4096 to 0. However, unlike on C64/C16/Plus4 this counter doesn't run while
the cursor is prompting for user input. Pressing repeated RETURNs or typing
`words`
will cause the counter to proceed, but it'll stop again on the input prompt.

This is because since version 3.9.6 the line input in EXPECT uses BASIN, i.e.
delegates the input wait to the CBM screen editor which only returns to
Forth code after RETURN is pressed, which means that until then PAUSE isn't
called and execution control isn't yielded to other tasks.

An alternative EXPECT is available in `x16input-tsk.fth` with the EXPECT
implementation of versions 3.9.5 and prior, similar to the C64/C16/Plus4
EXPECT, which implements the input loop in Forth, with a PAUSE call. On the
X16 this has the disadvantages that it uses a Kernal variable that could change
between Kernal versions, and has a bug that often leaves inverted spaces after
backspace is pressed. This being said,
```
include x16input-tsk.fth
```
after starting `taskdemo-x16.fth` will cause the counter to start running even
while waiting for input.

Typing `keyboard` will activate the original X16 EXPECT again and stop the
counter; typing `tasker-keyboard` will reactivate the task-switching EXPECT
and start the counter again.

## Implementation

As mentioned, the multitasker uses cooperative multitasking where
execution control is yielded via the word PAUSE. PAUSE saves the current state
of the currently active task and calls the task switcher. The state of a task
consists of the values of the Instruction Pointer (IP),
Return stack Pointer (RP) and Stack Pointer (SP).

The task switcher consists of a closed round-robin loop made up of the
first 6 bytes of the user area of each task.
See the picture [Tasks' user areas](#tasks-user-areas) in the
[Memory map](#memory-map) section.
Each task's user area starts with a machine code jump to
the next task's user area (`JMP XXXX` in the picture), followed by
`JSR wake`. `wake` is the task wake-up routine which sets UP to the task's
user area address calculated from return address left by the `jsr`,
restores SP from `user area + 6`and then restores RP and IP from the task's
data stack. The task's state is thereby restored, and the next iteration of
`NEXT` will call the task's next word, the one following the `PAUSE` that was
last invoked by the task.

A trick is employed to switch a task between active and inactive.
The JMP XXXX instruction at the user area's start, as described above, marks
an inactive task. When the round-robin loop reaches the task, the JMP
immediately forwards execution to the next task; the `JSR wake` is never
reached. For an active task, the JMP opcode is replaced by a BIT opcode (0x2c),
so that, when the task is jumped to, execution does reach the `JMP wake`
instruction, and the task runs until its reaaches the next `PAUSE`, which takes
the address of the BIT instruction for an indirect jump to the next task.

`SINGLETASK` changes `PAUSE` into a fast no-op so that no task change at all
takes place when `PAUSE` is called. This is the default of a VolksForth
system without loaded multitasker. `MULTITASK` enables the task-switching
behaviour of `PAUSE`.

The system supports the multitasker by invoking `PAUSE` during many I/O
operations such as `KEY`, `TYPE` and `BLOCK`. In many situations this is
already sufficient for a task (e.g. the printer pooler) to run smoothly.
In other situations a suitable placement of `PAUSE` calls within foreground
or background task code may be useful.

Tasks are created in the dictionary of the foreground or console task. Each
task has its own user area with a copy of the user variables.
The implementation of the system is, however, simplified through the
restriction that only the console task can interpret or compile input text.
There is e.g. only one vocabulary search order across the system; if one task
changes the search order, this affects all other tasks, too. But this is not
really disturbing, since only the console task should use the search order
anyway.

Incidentally, it is possible to forget active tasks: `FORGET` removes all
tasks from the round-robin loop that are located in the dictionary range to
forget. This can still go wrong, though, if the forgotten task holds a
"Semaphor" (see below). Semaphores are not released during forgetting,
and the associated device will remain blocked.

Finally, it should be mentioned that when invoking a task name, the address
of the task's user area will be placed on the stack.

## Memory map

The memory used by VolksForth ranges from
`ORIGIN` to `LIMIT` . Below `ORIGIN` are the
Kernal variables, the screen memory and a single line of
BASIC with a `SYS` command that starts VolksForth.
Beyond `LIMIT` are I/O ports and the Kernal ROM.

Just below `LIMIT` the block buffers are stored; each with a size
of 1 Kbyte tall. When the system starts, as many buffers are
allocated as fit between `R0` and `LIMIT`.

The rest of the system, located between `ORIGIN` and `RO`, consists of
two areas. The upper area contains the return stack (growing downwards,
starting at `RO @`) and the user area (growing upwards, starting at `UP@`).

The dictionary and the data stack plus the heap occupy the other area.
Heap and data stack grow downwards, the heap from `UP@` and the data stack
from `S0 @`.
When the heap grows, the data stack is automatically moved downwards.
New words are stored in the dictionary, which is the fastest-growing
part of the system during compilation. Therefore the system automatically
checks that dictionary and data stack don't collide.

The Bootarea finally contains the initial values of the user
variables, which is copied during the cold start from there into the
console task's user area.

#### Overall memory map

```
0xFFFF  -> ╔════════════════════╗
           ║   I/O and ROM      ║
           ║ (starts at 0x8000, ║
           ║  0x9F00, 0xD000 or ║
           ║  0xFD00 depending  ║
           ║  on platform)      ║
  limit -> ╠════════════════════╣
           ║     buffers        ║
first @ -> ╠════════════════════╣
           ║     (unused)       ║
   r0 @ -> ╠════════════════════╣ ─────
           ║   return stack     ║   ⋀
   rp@  -> ╠═════════════════|══╣   |
           ║                 ⋁  ║
           ║       free         ║  rlen
           ║                 ⋀  ║
           ╠═════════════════|══╣   |
           ║     user area      ║   ⋁
   up@  -> ╠════════════════════╣ ─────
           ║       heap         ║   ⋀
   heap -> ╠════════════════════╣   |
           ║ (stack underflow)  ║
   s0 @ -> ╠════════════════════╣
           ║       stack        ║
   sp@  -> ╠═════════════════|══╣  slen
           ║                 ⋁  ║
           ║       free         ║
           ║                 ⋀  ║
   here -> ╠═════════════════|══╣
           ║                    ║
           ║     dictionary     ║
           ║                    ║
           ╠════════════════════╣   |
           ║     boot area      ║   ⋁
 origin -> ╠════════════════════╣ ─────
           ║   10 SYS (2064)    ║       SYS (4112) on the C16
           ╠════════════════════╣
           ║   video memory     ║
           ╠════════════════════╣
           ║ Kernal vars        ║
           ║ 6502 ZP, stack     ║
           ╚════════════════════╝
```

#### 6502 zero page

The system also uses zero page memory, with the inner interpreter `NEXT`
and the stack pointers `RP` and `SP`, among others.


|Label  |  C64  |  C16  |  X16  |
|-------|-------|-------|-------|
|N      |0x0029 |0x0029 |0x0067 |
|W      |0x0021 |0x0021 |0x006F |
|IP     |0x000E |0x000E |0x005C |
|NEXT   |0x0009 |0x0009 |0x0057 |
|SP     |0x0007 |0x0007 |0x0055 |
|Put A  |0x0006 |0x0006 |0x0054 |
|UP     |0x0004 |0x0004 |0x0052 |
|RP     |0x0002 |0x0002 |0x0050 |



#### Memory map of a task
```
r0 @ -> ╔════════════════════╗ ─────
        ║   return stack     ║   ⋀
rp@  -> ╠═════════════════|══╣   |
        ║                 ⋁  ║
        ║       free         ║  rlen
        ║                 ⋀  ║
        ╠═════════════════|══╣   |
        ║     user area      ║   ⋁
up@  -> ╠════════════════════╣ ─────
        ║       heap         ║   ⋀
heap -> ╠════════════════════╣   |
        ║ (stack underflow)  ║
s0 @ -> ╠════════════════════╣
        ║       stack        ║
sp@  -> ╠═════════════════|══╣  slen
        ║                 ⋁  ║
        ║       free         ║
        ║                 ⋀  ║
here -> ╠═════════════════|══╣   |
        ║     dictionary     ║   ⋁
        ╚════════════════════╝ ─────
```
There's a small unused area of 6 bytes between stack and heap to prevent heap
corruption in case of a small stack underrun.

And typically, the dictionary of any task but the console task will be empty,
as that is where the outer interpreter is running which usally populates the
dictionary through definitions. However, `dp` is a user variable, so each task
has its own `here`, and if a task calls `allot`, the memory will be allocated
in its own dictionary.

#### Tasks' user areas

A task is characterized by the address of its user area. The tasker's round
robin loop consists of


```
            task 3: jmp XXXX => task is sleeping

          │    ...    │       task3 + 6
          ├───────────┤
          │ jsr wake  │       task3 + 3
          ├───────────┤
 ┌> -> -> │ jmp XXXX  │-> ┐   task3 + 0
 ⋀        └───────────┘   ⋁
 |     ┌ <- <- <-- <- <- <┘
 ⋀     ⋁
 |     |    task 2: bit XXXX => task is active
 ⋀     ⋁
 |     |  │    ...    │       task2 + 6
 ⋀     ⋁  ├───────────┤
 |     |  │ jsr wake  │       task2 + 3
 ⋀     ⋁  ├───────────┤
 |     └> │ bit XXXX  │-> ┐   task2 + 0
 ⋀        └───────────┘   ⋁
 |     ┌ <- <- <-- <- <- <┘
 ⋀     ⋁
 |     |    task 1: bit XXXX => task is active
 ⋀     ⋁
 |     |  │    ...    │       task1 + 6
 ⋀     ⋁  ├───────────┤
 |     |  │ jsr wake  │       task1 + 3
 ⋀     ⋁  ├───────────┤
 |     └> │ bit XXXX  │-> ┐   task1 + 0
 ⋀        └───────────┘   ⋁
 └ <- <- <- <- <-- <- <- <┘
```


## Glossary

#### 's

#### `'s  ( Tadr -- usradr )` "tick-s"

`'s  ( Tadr -- usradr )` "tick-s"
