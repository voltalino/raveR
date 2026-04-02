## R CMD check results

0 errors | 0 warnings | 2 notes

The 2 NOTEs are:
* "New submission" — expected for a first submission.
* "unable to verify current time" — transient network check, not a package issue.

## Test environments

* local macOS Sequoia 15.7.4, R 4.5.2
* win-builder (R-devel)

## Resubmission

This is a resubmission. In this version I have:

* Replaced the one remaining `\dontrun{}` (in `export_mp3()`) with
  `\donttest{}`. The example already guards with `if (check_ffmpeg())`
  so it runs gracefully even when FFmpeg is absent.

* Interactive/playback functions (`playR()`, `stopR()`, `play_audio()`,
  `stop_audio()`, `pause_audio()`, `resume_audio()`, `wait_audio()`,
  `PlaybackController`) use `if (interactive()) {}`.

* Fast, pure-computation examples (music theory, constants, sequencer
  pattern creation, swing/timing, metrics) are unwrapped and run
  directly.

* Audio synthesis examples that take > 5 sec use `\donttest{}`.

### Previous resubmission changes (still in place)

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
