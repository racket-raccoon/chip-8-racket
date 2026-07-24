# TODO

The current ROM collection runs successfully. The remaining work is mostly
correctness at the edges, broader compatibility, and GUI convenience.

## Core correctness

Do these before calling the interpreter generally complete.

- [ ] Fix `FX0A` key-wait behavior.
  - Do not discard a key event merely because another instruction executes
    before `FX0A`.
  - Halt until a new key is pressed, then wait for it to be released before
    resuming.
  - Keep the delay and sound timers running while the CPU is waiting.
  - The Timendus keypad test currently reports `NOT RELEASED`.

- [ ] Add the original CHIP-8 display-wait/vblank behavior.
  - Add a compatibility parameter such as `draw-waits-for-vblank?`.
  - In original COSMAC VIP mode, allow at most one `DXYN` draw per 60 Hz
    display frame.
  - The Timendus quirks test currently detects display wait as `OFF`; its
    other implemented quirk checks pass.

- [ ] Make GUI timing use elapsed time instead of a nominal 16 ms interval.
  - `(quotient 1000 60)` produces 16 ms, which is 62.5 Hz rather than 60 Hz.
  - Use a monotonic-clock accumulator for accurate 60 Hz timers.
  - Accumulate CPU work separately so `opcodes/second` stays accurate when a
    GUI callback is early, late, or skipped.

## GUI and quality of life

- [x] Add an Open ROM dialog instead of hard-coding the ROM path.
- [ ] Add compatibility presets:
  - Original CHIP-8
  - Modern CHIP-8
  - Super-CHIP quirks
- [ ] Add an Advanced settings section for individual compatibility options.
- [ ] Allow changing `opcodes/second` in the settings panel.
- [ ] Read or display the matching per-ROM `.txt` setup note when loading a
  ROM.
- [ ] Add pause, reset, and restart controls.

## Optional compatibility expansion

- [ ] Add a clipping/wrapping compatibility option. Clipping currently works
  correctly, but wrapping is not configurable.
- [ ] Replace `load-store-increments-i?` with a three-way memory mode:
  - `I += X + 1` for original CHIP-8
  - `I += X` for CHIP-48 / legacy Super-CHIP
  - leave `I` unchanged for Super-CHIP 1.1
- [ ] Consider full Super-CHIP support. The current Super-CHIP profile only
  implements its base-CHIP-8 opcode quirks.
  - 128 x 64 high-resolution mode
  - `00CN`, `00FB`, and `00FC` scrolling
  - `00FD`, `00FE`, and `00FF`
  - `DXY0` 16 x 16 sprites
  - `FX30` large font
  - `FX75` and `FX85` RPL flags
- [ ] Replace repeated system bells with a proper sustained buzzer tone.
- [ ] Add friendly errors for oversized ROMs, invalid memory access, and an
  out-of-range program counter.

## Validation

- [ ] Re-run the maintained Timendus test suite after core changes:
  <https://github.com/Timendus/chip8-test-suite>
- [ ] Keep the existing unit suite passing (currently 55 tests).
- [ ] Re-smoke-test every ROM using the configuration in its sidecar note.
- [ ] Use the CHIP-8 database when adding new ROM profiles:
  <https://github.com/chip-8/chip-8-database>
