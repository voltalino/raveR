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

* Reduced exported namespace to three user-facing functions: `composeR()`,
  `playR()`, and `stopR()`. All other functions are now internal.

* Moved `watcher` from Imports to Suggests. It is only needed for live
  file watching (`playR()`). A `requireNamespace()` check provides a clear
  error message if watcher is not installed.

* Replaced blanket `import(R6)`, `import(later)`, `import(watcher)` with
  explicit `::` calls. NAMESPACE now contains only selective
  `importFrom()` entries.

* Removed redundant `Author:` / `Maintainer:` fields (already covered by
  `Authors@R`).

* Quoted 'deep house' as a non-standard term in the Title field.

* Added a vignette explaining the workflow and code-to-music mapping.

* Replaced the one remaining `\dontrun{}` (in `export_mp3()`) with
  `\donttest{}`. The example already guards with `if (check_ffmpeg())`
  so it runs gracefully even when FFmpeg is absent.

* Interactive/playback functions use `if (interactive()) {}`.

### Previous resubmission changes (still in place)

* Added `skip_on_cran()` to 48 tests that perform heavy audio synthesis.
  This reduces test time from ~285s to ~30s. The remaining 599 tests still
  provide good coverage of core logic.

## Notes

This is a new package submission.

### Package purpose

raveR transforms R scripts into 'deep house' music by analyzing code
structure (functions, control flow, complexity) and mapping it to musical
parameters. The output is deterministic — the same script always produces
the same audio.

### System requirements

* FFmpeg is optional, only needed for MP3 export
* `watcher` (in Suggests) is optional, only needed for live mode (`playR()`)
* Audio playback uses the 'audio' package (works on macOS, Windows, Linux)
