#' @title Waveform Synthesis Functions
#' @description Functions for generating basic waveforms (sine, sawtooth, square)
#'   with standardized parameters. All functions use 32-bit float internally
#'   for processing headroom, and return tuneR Wave objects.
#' @name waveform-synthesis
NULL

#' Generate a Sine Wave
#'
#' Creates a sine wave at the specified frequency and duration.
#' Uses tuneR::sine() internally with standardized 32-bit float format.
#'
#' @param freq Frequency in Hz (e.g., 440 for A4)
#' @param duration_sec Duration in seconds
#' @param sample_rate Sample rate in Hz (default: 44100)
#'
#' @return A tuneR Wave object (mono, 32-bit float)
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate 1 second of 440 Hz (A4)
#' wave <- raver_sine(440, 1)
#' }
raver_sine <- function(freq, duration_sec, sample_rate = SAMPLE_RATE) {
  duration_samples <- as.integer(duration_sec * sample_rate)
  tuneR::sine(
    freq = freq,
    duration = duration_samples,
    samp.rate = sample_rate,
    bit = BIT_DEPTH
  )
}

#' Generate a Sawtooth Wave
#'
#' Creates a sawtooth wave at the specified frequency and duration.
#' Sawtooth waves have a bright, buzzy timbre suitable for bass and leads.
#' Uses tuneR::sawtooth() internally with standardized 32-bit float format.
#'
#' @param freq Frequency in Hz
#' @param duration_sec Duration in seconds
#' @param sample_rate Sample rate in Hz (default: 44100)
#' @param reverse If TRUE, generates an inverted (reverse) sawtooth
#'
#' @return A tuneR Wave object (mono, 32-bit float)
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate 1 second of 110 Hz sawtooth (bass range)
#' wave <- raver_sawtooth(110, 1)
#'
#' # Inverted sawtooth
#' wave_inv <- raver_sawtooth(110, 1, reverse = TRUE)
#' }
raver_sawtooth <- function(freq, duration_sec, sample_rate = SAMPLE_RATE, reverse = FALSE) {
  duration_samples <- as.integer(duration_sec * sample_rate)
  tuneR::sawtooth(
    freq = freq,
    duration = duration_samples,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    reverse = reverse
  )
}

#' Generate a Square Wave
#'
#' Creates a square wave at the specified frequency and duration.
#' Square waves have a hollow, clarinet-like timbre suitable for pads.
#' Uses tuneR::square() internally with standardized 32-bit float format.
#'
#' @param freq Frequency in Hz
#' @param duration_sec Duration in seconds
#' @param sample_rate Sample rate in Hz (default: 44100)
#' @param duty Duty cycle (0.0 to 1.0). Default 0.5 = 50% (symmetric square)
#'
#' @return A tuneR Wave object (mono, 32-bit float)
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate 1 second of 220 Hz square wave
#' wave <- raver_square(220, 1)
#'
#' # Narrow pulse (25% duty cycle)
#' wave_pulse <- raver_square(220, 1, duty = 0.25)
#' }
raver_square <- function(freq, duration_sec, sample_rate = SAMPLE_RATE, duty = 0.5) {
  duration_samples <- as.integer(duration_sec * sample_rate)
  tuneR::square(
    freq = freq,
    duration = duration_samples,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    up = duty
  )
}

#' Create a Standardized Wave Object
#'
#' Helper function to create Wave objects from raw sample data with
#' consistent format settings (32-bit float, specified sample rate).
#' Used by the buffer architecture and for custom waveform generation.
#'
#' @param samples Numeric vector of sample values (typically -1.0 to 1.0)
#' @param sample_rate Sample rate in Hz (default: 44100)
#'
#' @return A tuneR Wave object (mono, 32-bit float)
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a custom waveform from raw samples
#' samples <- sin(2 * pi * 440 * (0:44099) / 44100)
#' wave <- create_wave(samples)
#' }
create_wave <- function(samples, sample_rate = SAMPLE_RATE) {
  tuneR::Wave(
    left = as.numeric(samples),
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}
