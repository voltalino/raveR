## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS Sequoia 15.7.3, R 4.5.2
* win-builder (R-devel)

## Resubmission

This is a resubmission. In this version I have:

* Changed "WAV/MP3 export" to "audio file export" in the DESCRIPTION to
  avoid the NOTE about possible spelling errors.

* Added `skip_on_cran()` to 48 tests that perform heavy audio synthesis
  (full track composition, multi-bar rendering, FFT analysis). This reduces
  test time from ~285s to ~30s. The remaining 599 tests still provide good
  coverage of core logic (analysis, mapping, error handling, validation,
  state management).

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
