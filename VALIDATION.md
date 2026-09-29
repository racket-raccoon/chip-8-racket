# Validation record

Manual validation results for the emulator, recorded so they can be
re-run after future changes. Unit tests cover opcode semantics
headlessly; this file records what the real ROMs do in the real GUI.

## How to reproduce

```sh
racket gui.rkt                # then load a ROM through File > Load ROM...
raco test emu.rkt scheduler.rkt config.rkt
```

Screenshots and framebuffer dumps for every result below were captured
under headless Xvfb with the `chip8-gui-xvfb` skill; the settings and
key presses were applied through the same code paths the GUI uses
(`with-compat`, `scheduler-set-speed!`, `chip-8-key-pressed!`).

## Timendus CHIP-8 test suite, v4.2

Suite release <https://github.com/Timendus/chip8-test-suite/releases/tag/v4.2>
(2024-08-30), suite `main` commit `742e9eac9f5d44466c1c9d83fcaa8158c6683cb1`
at test time. The individual test binaries are vendored in
`roms/timendus-v4.2/`:

| File | SHA-256 (first 16) |
| --- | --- |
| 3-corax+.ch8 | `1c7e14eae14d6d5e` |
| 4-flags.ch8 | `f00ddadd37bc8784` |
| 5-quirks.ch8 | `d839350268a3e73c` |
| 6-keypad.ch8 | `558902b0e406bb97` |

### Opcode test (3-corax+.ch8)

Modern preset (720 opcodes/second). Every opcode group in the Corax+
grid shows its pass mark, no failures. Verified twice: before and
after the FX0A wait-state fix recorded below.

### Flags test (4-flags.ch8)

Modern preset. All VF flag checks (HAPPY rows 1-7 and E, CARRY rows
4-7 and E, OTHER FE) show pass marks.

### Keypad test (6-keypad.ch8)

Modern preset, keys driven through the emulator's own key handlers.

- **FX0A (menu 3, GET KEY): ✓ ALL GOOD.** FX0A halts, keeps the delay
  timer counting, latches the pressed key, and stores it on release.
- **EX9E (menu 1, KEY DOWN):** holding all 16 keys marks every cell of
  the test grid, so held-key reads work for every key.
- EXA1 (menu 2, KEY UP) is covered by the unit tests
  ("EXA1 skips only when the key in VX is not held") but was not
  screenshotted.

### Quirks test (5-quirks.ch8, platform 1 CHIP-8)

| Quirk | Original preset | Modern preset | Modern expected |
| --- | --- | --- | --- |
| VF RESET | ON, match | OFF, mismatch | OFF |
| MEMORY | ON, match | ON, match | ON (I += X + 1) |
| DISP. WAIT | ON, match | OFF, mismatch | OFF |
| CLIPPING | ON, match | ON, match | ON |
| SHIFTING | OFF, match | OFF, match | OFF (uses VY) |
| JUMPING | OFF, match | OFF, match | OFF (uses V0) |

The mismatch marks in the Modern column are the ROM comparing against
the original CHIP-8 platform, which is exactly what a modern profile
should differ in. Both presets report the documented matrix.

### Not run

- **7-beep.ch8**: requires the sustained-buzzer milestone (audio is
  still repeated system bells).
- **8-scrolling.ch8**: requires SUPER-CHIP 128 x 64 display support.

## FX0A wait-state deviation from the implementation plan

The plan suggested that FX0A should immediately consume a key press
and release that fully completed before FX0A started. Validation
showed that this makes the Timendus keypad test report NOT HALTING:
its own menu leaves a completed press pending when the FX0A test
begins, so FX0A returned without halting. The plan's own research
section requires FX0A to "halt until a new press", so the shipped
behavior is:

- a press **still held** when FX0A starts is latched and completes on
  release;
- a press **already released** before FX0A started is discarded, and
  FX0A waits for a genuinely new press.

The unit test "a press and release completed before FX0A does not
satisfy it" pins this behavior.

## Bundled ROM smoke tests

Defaults are "Custom" settings (the startup configuration); profiles
are applied through the settings dialog presets named below.

| ROM | Profile | Speed | Observed |
| --- | --- | --- | --- |
| brix.ch8 | Custom | 900 | Brick wall, paddle, ball in play; no errors |
| ibm-logo.ch8 | Custom | 1800 | IBM logo draws and animates |
| kaleidoscope.ch8 | Custom | 900 | Draws outward from the center |
| keypad-test.ch8 | SUPER-CHIP 1.1 (quirks only) | 1800 | Clean static keypad grid; keys highlight |
| maze.ch8 | Original CHIP-8 | 900 | Full maze pattern draws and regenerates |
| particle.ch8 | Custom | 900 | Title screen and falling particles |
| pong.ch8 | Custom | 900 | Court, scores, paddles moved with keys, ball in flight |
| sierpinski.ch8 | Custom | 900 | Triangle grows from the top vertex |
| space-invaders.ch8 | Custom | 1800 | Title screen renders |
| stars.ch8 | Custom | 1800 | Starfield draws |
| trip8.ch8 | Custom | 900 | Title screen renders |

Notes:

- **keypad-test.ch8** is a SUPER-CHIP test. Under base-CHIP-8 shift
  quirks its control state corrupts and it eventually executes sprite
  data as opcodes, exactly as its sidecar note warns; the emulator
  then shows the friendly "machine-language subroutine" error box with
  the PC and opcode, and the GUI stays usable. Under the SUPER-CHIP
  1.1 (quirks only) preset it runs clean.
- Input was exercised on pong (paddle keys) and keypad-test; the other
  ROMs are animation-driven, and "observed" reflects several seconds
  of unattended running.
