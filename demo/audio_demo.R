# demo/audio_demo.R
# RaveR Audio Foundation Demo
#
# This demo verifies your audio setup works correctly.
# You should hear several tones and see export files created.
#
# Usage:
#   devtools::load_all(".")
#   source("demo/audio_demo.R")
#
# Or with the package installed:
#   library(raveR)
#   source(system.file("demo", "audio_demo.R", package = "raveR"))

cat("\n")
cat("==============================================\n")
cat("     RaveR Audio Foundation Demo\n")
cat("==============================================\n")
cat("\n")
cat("This demo will:\n")
cat("  1. Play three waveform types (sine, sawtooth, square)\n")
cat("  2. Play a short melody using pre-rendered buffers\n")
cat("  3. Export the melody to WAV and MP3 files\n")
cat("\n")
cat("Make sure your speakers/headphones are connected!\n")
cat("\n")
cat("Press [Enter] to begin...\n")
readline()

# --- Part 1: Individual Waveforms ---

cat("\n")
cat("--- Part 1: Waveform Types ---\n")
cat("\n")

cat("Playing SINE wave (440 Hz) - pure, fundamental tone...\n")
sine_wave <- raver_sine(440, 0.8)
play_audio(sine_wave)
wait_audio()
Sys.sleep(0.3)

cat("Playing SAWTOOTH wave (220 Hz) - bright, buzzy bass...\n")
saw_wave <- raver_sawtooth(220, 0.8)
play_audio(saw_wave)
wait_audio()
Sys.sleep(0.3)

cat("Playing SQUARE wave (330 Hz) - hollow, clarinet-like...\n")
square_wave <- raver_square(330, 0.8)
play_audio(square_wave)
wait_audio()
Sys.sleep(0.3)

cat("\n")
cat("All three waveforms played successfully!\n")
cat("\n")

# --- Part 2: Pre-Rendered Buffer Architecture ---

cat("--- Part 2: Pre-Rendered Buffer (Melody) ---\n")
cat("\n")
cat("Creating a melody using the buffer architecture...\n")
cat("This demonstrates GC-pause-free playback.\n")
cat("\n")

# Note frequencies (A minor pentatonic starting at A4)
note_freqs <- c(
  A4 = 440.00,
  C5 = 523.25,
  D5 = 587.33,
  E5 = 659.25,
  G5 = 783.99,
  A5 = 880.00
)

# Simple melody: A4 - C5 - D5 - E5 - G5 - A5 - G5 - E5
melody_notes <- c("A4", "C5", "D5", "E5", "G5", "A5", "G5", "E5")
note_duration <- 0.3
gap_duration <- 0.05

# Pre-render the entire melody buffer
melody_buffer <- render_buffer(
  duration_sec = length(melody_notes) * (note_duration + gap_duration),
  content_fn = function() {
    # Build melody by concatenating notes with gaps
    waves <- list()
    for (i in seq_along(melody_notes)) {
      note_name <- melody_notes[i]
      freq <- note_freqs[note_name]
      note <- raver_sine(freq, note_duration)
      gap <- create_silence(gap_duration)
      waves <- c(waves, list(note, gap))
    }
    do.call(bind_waves, waves)
  }
)

cat("Melody pre-rendered. Playing now...\n")
cat("\n")

play_audio(melody_buffer)
wait_audio()

cat("\n")
cat("Melody played successfully!\n")
cat("(No clicks or pops = buffer architecture working correctly)\n")
cat("\n")

# --- Part 3: Export ---

cat("--- Part 3: Audio Export ---\n")
cat("\n")

# Determine output directory (project root or temp)
output_dir <- getwd()
if (!file.access(output_dir, 2) == 0) {
  output_dir <- tempdir()
  cat(sprintf("Note: Using temp directory for exports: %s\n", output_dir))
}

wav_path <- file.path(output_dir, "raver_demo_melody.wav")
mp3_path <- file.path(output_dir, "raver_demo_melody.mp3")

# Export WAV
cat("Exporting to WAV (16-bit, 44.1kHz)...\n")
export_wav(melody_buffer, wav_path)
wav_size <- file.size(wav_path)
cat(sprintf("  Created: %s\n", wav_path))
cat(sprintf("  Size: %.1f KB\n", wav_size / 1024))
cat("\n")

# Export MP3 (if FFmpeg available)
if (suppressWarnings(check_ffmpeg())) {
  cat("Exporting to MP3 (192 kbps)...\n")
  export_mp3(melody_buffer, mp3_path)
  mp3_size <- file.size(mp3_path)
  cat(sprintf("  Created: %s\n", mp3_path))
  cat(sprintf("  Size: %.1f KB\n", mp3_size / 1024))
  cat("\n")
  cat(sprintf("Compression ratio: %.1fx smaller with MP3\n", wav_size / mp3_size))
} else {
  cat("Skipping MP3 export (FFmpeg not available)\n")
  cat("Install FFmpeg to enable MP3 export:\n")
  cat("  macOS: brew install ffmpeg\n")
  cat("  Ubuntu: sudo apt-get install ffmpeg\n")
}
cat("\n")

# --- Summary ---

cat("==============================================\n")
cat("              Demo Complete!\n")
cat("==============================================\n")
cat("\n")
cat("What was verified:\n")
cat("  [x] Waveform synthesis (sine, sawtooth, square)\n")
cat("  [x] Cross-platform audio playback\n")
cat("  [x] Pre-rendered buffer architecture\n")
cat("  [x] WAV export (16-bit, 44.1kHz)\n")
if (suppressWarnings(check_ffmpeg())) {
  cat("  [x] MP3 export (192 kbps via FFmpeg)\n")
} else {
  cat("  [ ] MP3 export (FFmpeg not installed)\n")
}
cat("\n")
cat("Exported files:\n")
cat(sprintf("  WAV: %s\n", wav_path))
if (suppressWarnings(check_ffmpeg())) {
  cat(sprintf("  MP3: %s\n", mp3_path))
}
cat("\n")
cat("Try playing the exported files in your system audio player!\n")
cat("\n")

# Cleanup prompt
cat("Would you like to delete the demo files? (y/n): ")
answer <- tolower(trimws(readline()))
if (answer == "y" || answer == "yes") {
  if (file.exists(wav_path)) unlink(wav_path)
  if (file.exists(mp3_path)) unlink(mp3_path)
  cat("Demo files deleted.\n")
} else {
  cat("Demo files kept.\n")
}

cat("\n")
cat("Thanks for trying RaveR!\n")
cat("\n")
