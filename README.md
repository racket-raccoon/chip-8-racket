# CHIP-8 emulator in Racket

A CHIP-8 interpreter with a Racket GUI, compatible-mode presets, and a
timed scheduler based on monotonic wall-clock time.

![Dependencies](https://img.shields.io/badge/racket-gui-blue)

## Running

You need a full Racket distribution (the program uses `racket/gui`).

```sh
make        # compile the Racket modules
make run    # compile if needed, then launch the GUI
```

You can also launch directly with `racket gui.rkt`. Use `make clean` to remove
Racket compilation artifacts.

Load a ROM with **File > Load ROM...**; the `roms/` directory has a
small library of public CHIP-8 programs and the vendored Timendus v4.2
test suite under `roms/timendus-v4.2/`.

## Controls

### Host keyboard to CHIP-8 keypad

```
CHIP-8 keypad          host keys

 1 2 3 C               1 2 3 4
 4 5 6 D               Q W E R
 7 8 9 E               A S D F
 A 0 B F               Z X C V
```

### Machine menu

- **Pause / Resume** freezes the CPU and both timers without touching
  machine state; resuming continues exactly where it stopped.
- **Reset** reloads the cached ROM bytes into a clean machine and
  leaves it paused at the initial state.
- **Restart** reloads the cached bytes and runs immediately.

The lifecycle controls are disabled until a ROM is loaded. Reset and
restart reuse the bytes cached at load time, so the file changing on
disk cannot make two resets behave differently.

## ROM setup notes

Every bundled ROM has an adjacent setup note: **File > ROM Info...** shows
`foo.txt` for `foo.ch8` as plain UTF-8 text. The notes describe the profile and
speed the ROM expects; they are displayed faithfully and never parsed or
applied automatically — you apply them through the settings dialog.

## Settings

**File > Settings...** edits a draft configuration and applies it
atomically when you press OK; Cancel discards every change.

- **Preset** — Custom, Original CHIP-8, Modern CHIP-8, and
  SUPER-CHIP 1.1 (quirks only). Selecting a preset fills every
  control, including the CPU speed; editing any single control drops
  the display back to Custom.
- **Opcodes per second** — a whole number from 60 to 10000. Invalid
  text is rejected when you confirm, never coerced.
- **Advanced** — the individual compatibility options:
  - FX1E sets VF on overflow
  - FX55/FX65 memory mode (`I += X + 1`, `I += X`, or unchanged)
  - logic operations clear VF
  - shift instructions use VY
  - BNNN jump uses VX
  - draws wait for vertical blank (one DXYN per 60 Hz frame)
  - wrap sprites at screen edges

The SUPER-CHIP 1.1 preset provides only its base-CHIP-8 opcode
quirks. The SUPER-CHIP extended opcodes (high-resolution display,
`00CN/00FB/00FC` scrolling, `00FD/00FE/00FF`, `DXY0`, `FX30`,
`FX75`/`FX85`) are **not implemented**.

## Supported opcodes

The complete base CHIP-8 set:

`00E0 00EE 1NNN 2NNN 3XNN 4XNN 5XY0 6XNN 7XNN 8XY0 8XY1 8XY2 8XY3
8XY4 8XY5 8XY6 8XY7 8XYE 9XY0 ANNN BNNN CXNN DXYN EX9E EXA1 FX07
FX0A FX15 FX18 FX1E FX29 FX33 FX55 FX65`

`0NNN` (machine-language subroutines) is unsupported and raises a
contextual error. Timers run at 60 Hz from monotonic time, the CPU
runs at the configured opcodes/second, and `FX0A` waits for a full
press-and-release cycle while the timers keep ticking.

## Errors

Bad ROMs cannot crash the application: oversized ROMs are rejected at
load, and invalid memory access, unknown opcodes, and stack
over/underflow pause the machine and show one dialog naming the
operation, the PC, and the opcode. Resume is disabled after an execution
fault because the failing instruction may already have advanced the PC;
the window stays open so you can load another ROM, reset, or restart.

## Tests

```sh
make test
# equivalent to: raco test emu.rkt scheduler.rkt config.rkt
```

`emu.rkt` covers opcode semantics, the FX0A wait state, drawing quirks,
and memory bounds; `scheduler.rkt` covers deadline scheduling with
injected timestamps; `config.rkt` covers the preset matrix, speed
validation, and parameter binding. `VALIDATION.md` records manual
results for the Timendus v4.2 suite and every bundled ROM.

## Project layout

```text
emu.rkt        machine state, opcode semantics, timers, input, drawing
scheduler.rkt  elapsed-time scheduling for CPU and 60 Hz events
config.rkt     compatibility presets and parameter binding
gui.rkt        windows, menus, settings dialog, rendering
```
