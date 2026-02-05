## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS Sequoia 15.7.3, R 4.5.2
* win-builder (R-devel)

## Resubmission

This is a resubmission. In this version I have:

* Removed all code that modified `.GlobalEnv` (specifically `.Random.seed`
  save/restore logic in `generate_glitch_effect()`, `raver_compose()`, and
  `raver_compose_section()`). Functions now set the seed without attempting
  to restore previous RNG state.

## Notes

This is a new package submission.

### Package purpose

raveR transforms R scripts into deep house music by analyzing code structure
(functions, control flow, complexity) and mapping it to musical parameters.
The output is deterministic - the same script always produces the same audio.

### System requirements

* FFmpeg is optional, only needed for MP3 export
* Audio playback uses the 'audio' package (works on macOS, Windows, Linux)

### Examples

All examples use `\dontrun{}` as they either produce audio output or require
file system access.
