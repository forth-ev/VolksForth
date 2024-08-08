
# The Multitasker

VolksForth comes with a simple but powerful multitasker which allows the creation of printer poolers, clocks, counters and other simple background tasks.

The main characteristic of this multitasker is that it uses cooperative, not
preemtive multitasking. This means that each task has to explicitly yield
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

As mentioned,
execution control is yielded via the word PAUSE. PAUSE saves the current state
of the currently active task and calls the task switcher. The state of a task
consists of the values of the Instruction Pointer (IP),
Return stack Pointer (RP) and Stack Pointer (SP).

The task switcher consists of a closed loop
(see picture). Each task contains a machine code jump on
the next task ("jmp XXXX" in the picture), followed by
"jsr (wake) in the picture. At the address, on
which the jump command aims, are located
Instructions of the next task. If this task is stopped,
there is also a machine code jump to the next task
made. If, on the other hand, the task is active,
the (non-executive) BIT command has been replaced. The
follows the call of the wake-up procedure. This procedure invites
state of task (consisting of SP ‘a RP and IP) and sets the
Userpointer (UP), so that it shows this task.


## Glossary

#### 's

#### `'s  ( Tadr -- usradr )` "tick-s"

`'s  ( Tadr -- usradr )` "tick-s"
