# raveR 0.1.0

Initial CRAN release.

## Features

* **Code Analysis**: Parse R scripts to extract structural information (functions,
  variables, control flow, cyclomatic complexity)

* **Music Generation**: Transform code metrics into deep house music with:
  - Deterministic output (same script = same music)
  - Multiple genres: deep_house, techno, ambient, drum_bass, house
  - BPM range 60-180 depending on genre

* **Live Coding**: Real-time music generation with file watching
  - `playR()` starts continuous playback
  - Music transitions smoothly when script is modified
  - `stopR()` halts playback

* **Audio Export**:

  - WAV export (lossless)
  - MP3 export (requires FFmpeg)

* **Synthesis Engine**:
  - 909-style drum kit (kick, snare, hihat, clap)
  - Filtered bass with octave patterns
  - Warm pad chords with ADSR envelopes
  - Percussive lead with delay effects

* **Musical Intelligence**:
  - Pentatonic scale mapping for pleasing melodies
  - Function-based motif generation
  - Chord progressions derived from code structure
  - Section-based arrangement (intro, build, drop, breakdown, outro)
