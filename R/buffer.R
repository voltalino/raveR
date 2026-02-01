#' @title Pre-Rendered Buffer Architecture
#' @description Functions for creating and managing pre-rendered audio buffers.
#'   This architecture is critical for working around R's garbage collection
#'   pauses (50-100ms) which would otherwise cause audio dropouts during
#'   real-time playback. By pre-rendering complete buffers before playback,
#'   GC pauses occur between buffer swaps rather than during audio output.
#' @name buffer-architecture
NULL

#' Create Silence
#'
#' Generates a Wave object containing silence for the specified duration.
#' Uses the 32-bit float workaround for tuneR's silence() function.
#'
#' @param duration_sec Duration in seconds
#' @param sample_rate Sample rate in Hz (default: 44100)
#'
#' @return A tuneR Wave object (mono, 32-bit float) containing silence
#' @export
#'
#' @details
#' Note: tuneR's silence() function has issues with 16-bit PCM directly.
#' This function always generates at 32-bit float (bit=32, pcm=FALSE)
#' for maximum compatibility. Use normalize_for_export() to convert
#' to 16-bit before WAV export.
#'
#' @examples
#' \dontrun{
#' # Create 0.5 seconds of silence
#' gap <- create_silence(0.5)
#'
#' # Create 2 seconds of silence at custom sample rate
#' gap_48k <- create_silence(2, sample_rate = 48000)
#' }
create_silence <- function(duration_sec, sample_rate = SAMPLE_RATE) {
  duration_samples <- as.integer(duration_sec * sample_rate)
  tuneR::silence(
    duration = duration_samples,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Render a Pre-Rendered Audio Buffer
#'
#' Creates a complete audio buffer by evaluating a content function.
#' This is the core of the GC-pause workaround: by generating audio
#' upfront before playback, garbage collection pauses happen during
#' rendering rather than during audio output.
#'
#' @param duration_sec Target duration in seconds for the buffer
#' @param content_fn A function that returns a Wave object to fill the buffer.
#'   The function should generate audio matching the specified duration.
#' @param sample_rate Sample rate in Hz (default: 44100)
#'
#' @return A complete pre-rendered tuneR Wave object
#' @export
#'
#' @details
#' The render_buffer function is designed for the pre-rendered buffer

#' architecture that works around R's garbage collection limitations.
#' R's stop-the-world GC can pause execution for 50-100ms or more,
#' which would cause clicks and dropouts in real-time audio.
#'
#' By rendering complete buffers (typically 4-8 seconds, or 1-4 bars
#' at 120 BPM) before playback begins, we ensure smooth audio output.
#' The content_fn is called once to generate the buffer content.
#'
#' @examples
#' \dontrun
#' # Pre-render a 4-second buffer with a 440 Hz sine wave
#' buffer <- render_buffer(4, function() raver_sine(440, 4))
#'
#' # Pre-render with multiple sounds
#' buffer <- render_buffer(2, function() {
#'   sine <- raver_sine(440, 1)
#'   square <- raver_square(220, 1)
#'   bind_waves(sine, square)
#' })
#' }
render_buffer <- function(duration_sec, content_fn, sample_rate = SAMPLE_RATE) {
  # Evaluate the content function to generate the audio
  content <- content_fn()

  # Validate that content is a Wave object

if (!inherits(content, "Wave")) {
    stop("content_fn must return a tuneR Wave object")
  }

  # Return the pre-rendered content
  # The caller is responsible for ensuring duration matches
  content
}

#' Normalize Wave for Export
#'
#' Converts a 32-bit internal Wave object to 16-bit PCM format
#' suitable for WAV file export. This is the final step before
#' writing audio to disk.
#'
#' @param wave A tuneR Wave object (typically 32-bit float)
#'
#' @return A tuneR Wave object in 16-bit PCM format
#' @export
#'
#' @details
#' Internal processing uses 32-bit float for headroom during mixing.
#' WAV export requires 16-bit PCM for maximum compatibility with
#' audio players. This function performs the conversion using
#' tuneR::normalize() with unit="16".
#'
#' @examples
#' \dontrun{
#' # Generate and normalize for export
#' wave <- raver_sine(440, 1)
#' export_wave <- normalize_for_export(wave)
#' tuneR::writeWave(export_wave, "output.wav")
#' }
normalize_for_export <- function(wave) {
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object")
  }
  tuneR::normalize(wave, unit = "16")
}

#' Bind Multiple Wave Objects
#'
#' Concatenates multiple Wave objects sequentially. Validates that
#' all waves have matching format (sample rate, bit depth, channels)
#' before binding to prevent cryptic tuneR errors.
#'
#' @param ... Wave objects to concatenate
#'
#' @return A single tuneR Wave object containing all input waves
#' @export
#'
#' @details
#' tuneR's bind() function requires all Wave objects to have identical
#' sample rate, bit depth, and channel count. This wrapper validates
#' formats upfront and provides clear error messages if mismatches
#' are detected.
#'
#' @examples
#' \dontrun{
#' # Concatenate three different tones
#' w1 <- raver_sine(440, 0.5)   # 0.5 sec of 440 Hz
#' w2 <- raver_sine(550, 0.5)   # 0.5 sec of 550 Hz
#' w3 <- raver_sine(660, 0.5)   # 0.5 sec of 660 Hz
#' melody <- bind_waves(w1, w2, w3)  # 1.5 sec total
#' }
bind_waves <- function(...) {
  waves <- list(...)

  if (length(waves) == 0) {
    stop("At least one Wave object is required")
  }

  # Validate all are Wave objects
  for (i in seq_along(waves)) {
    if (!inherits(waves[[i]], "Wave")) {
      stop(sprintf("Argument %d is not a Wave object", i))
    }
  }

  # Check format consistency
  first <- waves[[1]]
  ref_samp_rate <- first@samp.rate
  ref_bit <- first@bit
  ref_stereo <- first@stereo

  for (i in seq_along(waves)[-1]) {
    w <- waves[[i]]
    if (w@samp.rate != ref_samp_rate) {
      stop(sprintf(
        "Sample rate mismatch: wave 1 has %d Hz, wave %d has %d Hz",
        ref_samp_rate, i, w@samp.rate
      ))
    }
    if (w@bit != ref_bit) {
      stop(sprintf(
        "Bit depth mismatch: wave 1 has %d-bit, wave %d has %d-bit",
        ref_bit, i, w@bit
      ))
    }
    if (w@stereo != ref_stereo) {
      stop(sprintf(
        "Channel mismatch: wave 1 is %s, wave %d is %s",
        if (ref_stereo) "stereo" else "mono",
        i,
        if (w@stereo) "stereo" else "mono"
      ))
    }
  }

  # All checks passed, bind the waves
  # Strip names to avoid issues with tuneR::bind's argument matching
  waves <- unname(waves)
  do.call(tuneR::bind, waves)
}
