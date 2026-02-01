# raveR

Transform R scripts into deep house music.

raveR analyzes your R code structure—functions, control flow, complexity—and maps it to musical parameters to generate listenable deep house tracks. Same code, same seed, same music.

## Installation

```r
# Install from GitHub
devtools::install_github("voltalino/raveR")
```

**Dependencies:** Requires the `tuneR`, `audio`, and `av` packages. MP3 export requires FFmpeg installed on your system.

## Quick Start

```r
library(raveR)

# Convert any R script to a WAV file
composeR("my_analysis.R", "output.wav")

# Or MP3 (requires FFmpeg)
composeR("my_analysis.R", "output.mp3")
```

## Live Mode

Start generative playback that responds to code changes in real-time:

```r
# Start live playback
playR("my_analysis.R")

# Edit and save my_analysis.R — the music transitions smoothly
# ...work on your code...

# Stop when done
stopR()
```

The R console remains interactive during playback.

## How It Works

raveR extracts musical parameters from your code:

| Code Feature | Musical Mapping |
|--------------|-----------------|
| Function count | Number of musical parts |
| Nesting depth | Harmonic complexity |
| Control flow | Pattern density |
| Variable count | Instrument layers |
| Cyclomatic complexity | Arrangement length |

The output has classic deep house characteristics:
- Four-on-the-floor kick pattern
- Hi-hats with swing
- Snare/clap on beats 2 and 4
- Filtered bass lines
- Warm pad chord progressions
- Structured arrangement (intro → build → drop → breakdown → outro)

## API

### `composeR(script_path, output_path, bpm = NULL, seed = NULL)`

Main function to create an audio file from an R script.

- `script_path` — Path to the R script
- `output_path` — Output file (.wav or .mp3)
- `bpm` — Tempo (default: auto-selected 118-124 based on file hash)
- `seed` — Random seed for reproducibility

### `playR(script_path, bpm = NULL)`

Start live generative playback with file watching.

### `stopR()`

Stop live playback.

## Examples

```r
# Basic usage
composeR("script.R", "track.wav")

# Custom BPM
composeR("script.R", "track.wav", bpm = 122)

# Reproducible output
composeR("script.R", "track.wav", seed = 42)
```

## License

MIT
