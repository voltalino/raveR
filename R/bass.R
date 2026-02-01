#' @title Bass Synthesizer with Filter
#' @description Filtered sawtooth bass synthesizer for deep house production.
#'   Creates punchy, filtered bass sounds with ADSR envelope shaping.
#' @name bass
NULL

#' Apply ADSR envelope to samples
#'
#' @description Internal helper that applies an Attack-Decay-Sustain-Release
#'   envelope to a sample vector.
#'
#' @param samples Numeric vector of audio samples
#' @param sample_rate Sample rate in Hz
#' @param attack Attack time in seconds
#' @param decay Decay time in seconds
#' @param sustain_level Sustain level from 0.0 to 1.0
#' @param release Release time in seconds
#'
#' @return Numeric vector of enveloped samples
#' @keywords internal
apply_adsr <- function(samples, sample_rate, attack, decay, sustain_level, release) {
  n_samples <- length(samples)

  # Convert times to sample counts
  attack_samples <- as.integer(attack * sample_rate)
  decay_samples <- as.integer(decay * sample_rate)
  release_samples <- as.integer(release * sample_rate)

  # Calculate sustain length (everything after attack+decay, before release)
  envelope_samples <- attack_samples + decay_samples + release_samples
  if (n_samples > envelope_samples) {
    sustain_samples <- n_samples - envelope_samples
  } else {
    sustain_samples <- 0L
    # Scale down the phases proportionally if duration is shorter than envelope
    total_requested <- attack_samples + decay_samples + release_samples
    if (total_requested > n_samples && total_requested > 0) {
      scale_factor <- n_samples / total_requested
      attack_samples <- as.integer(attack_samples * scale_factor)
      decay_samples <- as.integer(decay_samples * scale_factor)
      release_samples <- n_samples - attack_samples - decay_samples
    }
  }

  # Build envelope
  envelope <- numeric(n_samples)
  pos <- 1L

  # Attack phase: 0 to 1
  if (attack_samples > 0) {
    attack_end <- min(pos + attack_samples - 1, n_samples)
    attack_length <- attack_end - pos + 1
    if (attack_length > 0) {
      envelope[pos:attack_end] <- seq(0, 1, length.out = attack_length)
      pos <- attack_end + 1
    }
  }

  # Decay phase: 1 to sustain_level
  if (pos <= n_samples && decay_samples > 0) {
    decay_end <- min(pos + decay_samples - 1, n_samples)
    decay_length <- decay_end - pos + 1
    if (decay_length > 0) {
      envelope[pos:decay_end] <- seq(1, sustain_level, length.out = decay_length)
      pos <- decay_end + 1
    }
  }

  # Sustain phase: hold at sustain_level
  if (pos <= n_samples && sustain_samples > 0) {
    sustain_end <- min(pos + sustain_samples - 1, n_samples)
    envelope[pos:sustain_end] <- sustain_level
    pos <- sustain_end + 1
  }

  # Release phase: sustain_level to 0
  if (pos <= n_samples) {
    release_length <- n_samples - pos + 1
    if (release_length > 0) {
      # Start from current level (may not have reached sustain_level)
      start_level <- if (pos > 1) envelope[pos - 1] else sustain_level
      envelope[pos:n_samples] <- seq(start_level, 0, length.out = release_length)
    }
  }

  samples * envelope
}

#' Apply lowpass filter to a Wave
#'
#' @description Applies a simple one-pole lowpass filter to a Wave object.
#'   This is computationally efficient and suitable for bass filtering.
#'
#' @param wave A Wave object to filter
#' @param cutoff_hz Cutoff frequency in Hz
#'
#' @return Filtered Wave object
#' @keywords internal
apply_lowpass_filter <- function(wave, cutoff_hz) {
  samples <- wave@left
  sample_rate <- wave@samp.rate

  # One-pole lowpass filter coefficient
  # alpha determines how much of the input vs feedback to use
  alpha <- cutoff_hz / (cutoff_hz + sample_rate / (2 * pi))

  # Apply filter
  n <- length(samples)
  if (n == 0) return(wave)

  output <- numeric(n)
  output[1] <- samples[1] * alpha

  for (i in 2:n) {
    output[i] <- (1 - alpha) * output[i - 1] + alpha * samples[i]
  }

  tuneR::Wave(
    left = output,
    samp.rate = sample_rate,
    bit = wave@bit,
    pcm = FALSE
  )
}

#' Generate a bass note
#'
#' @description Creates a filtered sawtooth bass note with ADSR envelope.
#'   This is the core sound for deep house bass lines.
#'
#' @param midi_note MIDI note number (36 = C2, typical bass range 24-48)
#' @param duration_sec Duration in seconds
#' @param velocity Velocity from 0.0 to 1.0, scales amplitude. Default 1.0.
#' @param cutoff_hz Lowpass filter cutoff frequency in Hz. Default 800.
#'
#' @return A Wave object containing the bass note
#'
#' @details
#' The bass sound uses a sawtooth wave as the source, which is rich in
#' harmonics. The lowpass filter removes high frequencies for a warm,
#' deep sound. The ADSR envelope (attack=0.005, decay=0.1, sustain=0.8,
#' release=0.1) provides a snappy attack with sustained body.
#'
#' @examples
#' \dontrun{
#' # C2 bass note, 0.5 seconds
#' bass_note <- raver_bass_note(36, 0.5)
#'
#' # A1 bass note with lower filter
#' deep_bass <- raver_bass_note(33, 1.0, cutoff_hz = 400)
#' }
#'
#' @export
raver_bass_note <- function(midi_note, duration_sec, velocity = 1.0, cutoff_hz = 800) {
  # Convert MIDI to frequency

freq <- raver_midi_to_freq(midi_note)

  # Generate sawtooth wave
  wave <- raver_sawtooth(freq, duration_sec, SAMPLE_RATE)

  # Apply lowpass filter
  wave <- apply_lowpass_filter(wave, cutoff_hz)

  # Apply ADSR envelope
  enveloped_samples <- apply_adsr(
    wave@left,
    SAMPLE_RATE,
    attack = 0.005,
    decay = 0.1,
    sustain_level = 0.8,
    release = 0.1
  )

  # Scale by velocity
  enveloped_samples <- enveloped_samples * velocity

  tuneR::Wave(
    left = enveloped_samples,
    samp.rate = SAMPLE_RATE,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Create a bass line
#'
#' @description Sequences multiple bass notes into a bass line.
#'   Handles rests (0 or NA) and variable note durations.
#'
#' @param notes Vector of MIDI note numbers. Use 0 or NA for rests.
#' @param durations Vector of durations in 16th notes. 1 = 16th, 2 = 8th, 4 = quarter.
#' @param bpm Beats per minute (tempo)
#' @param velocity Velocity from 0.0 to 1.0 for all notes. Default 0.9.
#' @param cutoff_hz Lowpass filter cutoff frequency in Hz. Default 800.
#'
#' @return A Wave object containing the complete bass line
#'
#' @details
#' The notes and durations vectors must have the same length. Each note
#' is generated with the corresponding duration (converted from 16th notes
#' to seconds based on BPM). Rests are rendered as silence.
#'
#' @examples
#' \dontrun{
#' # Classic house bass pattern: C2, rest, C3, C2
#' notes <- c(36, 0, 48, 36)
#' durations <- c(4, 4, 4, 4)  # All quarter notes
#' bass <- raver_bass_line(notes, durations, bpm = 120)
#'
#' # Syncopated bass line
#' notes <- c(36, 0, 36, 48, 0, 36, 36, 48)
#' durations <- c(2, 2, 2, 2, 2, 2, 2, 2)  # All 8th notes
#' synco_bass <- raver_bass_line(notes, durations, bpm = 124)
#' }
#'
#' @export
raver_bass_line <- function(notes, durations, bpm, velocity = 0.9, cutoff_hz = 800) {
  if (length(notes) != length(durations)) {
    stop("notes and durations must have the same length")
  }

  if (length(notes) == 0) {
    return(create_silence(0, SAMPLE_RATE))
  }

  # Calculate 16th note duration in seconds
  beat_sec <- 60 / bpm
  sixteenth_sec <- beat_sec / 4

  # Generate each note
  waves <- vector("list", length(notes))

  for (i in seq_along(notes)) {
    duration_sec <- durations[i] * sixteenth_sec

    if (is.na(notes[i]) || notes[i] == 0) {
      # Rest - generate silence
      waves[[i]] <- create_silence(duration_sec, SAMPLE_RATE)
    } else {
      # Generate bass note
      waves[[i]] <- raver_bass_note(notes[i], duration_sec, velocity, cutoff_hz)
    }
  }

  # Concatenate all waves
  do.call(bind_waves, waves)
}

#' Create classic house octave bass pattern
#'
#' @description Creates the classic house bass pattern with root note and
#'   octave jumps. This pattern is fundamental to deep house.
#'
#' @param root_midi MIDI note number for the root note (bass range recommended)
#' @param pattern Character vector specifying the pattern. Valid values:
#'   "root" (plays root note), "octave" (plays root + 12), "rest" (silence)
#' @param bpm Beats per minute (tempo)
#' @param velocity Velocity from 0.0 to 1.0. Default 0.9.
#' @param cutoff_hz Lowpass filter cutoff. Default 800.
#'
#' @return A Wave object containing the bass pattern
#'
#' @details
#' Each element of the pattern is a 16th note. A typical house bass uses
#' patterns like c("root", "rest", "octave", "rest") repeated.
#'
#' @examples
#' \dontrun{
#' # Classic octave pattern on C2
#' pattern <- c("root", "rest", "octave", "rest")
#' bass <- raver_bass_octave_pattern(36, rep(pattern, 4), bpm = 120)
#' }
#'
#' @export
raver_bass_octave_pattern <- function(root_midi, pattern, bpm,
                                       velocity = 0.9, cutoff_hz = 800) {
  # Convert pattern to MIDI notes
  notes <- sapply(pattern, function(p) {
    p <- tolower(p)
    if (p == "root") {
      root_midi
    } else if (p == "octave") {
      root_midi + 12L
    } else {
      0L  # rest
    }
  }, USE.NAMES = FALSE)

  # All 16th notes
  durations <- rep(1L, length(notes))

  raver_bass_line(notes, durations, bpm, velocity, cutoff_hz)
}
