#' @title Pad Synthesizer with Chord Support
#' @description Warm pad synthesizer for deep house production.
#'   Creates lush, detuned pad sounds with slow envelopes for
#'   harmonic warmth and atmosphere. Includes reverb for that
#'   classic deep house spacious sound.
#' @name pads
NULL

# =============================================================================
# Reverb Effect
# =============================================================================

#' Apply reverb effect to audio
#'
#' @description Creates a spacious reverb effect by adding multiple delayed
#'   copies of the signal with exponential decay. Essential for the classic
#'   deep house pad sound.
#'
#' @param wave A Wave object to apply reverb to
#' @param decay Initial decay amount (0-1). Each reflection is quieter.
#'   Default 0.35.
#' @param delays_ms Numeric vector of delay times in milliseconds.
#'   Default c(50, 100, 150, 200, 280, 380) for a lush reverb tail.
#' @param decay_rate Rate at which each reflection gets quieter.
#'   Default 0.55.
#' @param wet_dry Wet/dry mix (0 = all dry, 1 = all wet). Default 0.4.
#'
#' @return A Wave object with reverb applied
#'
#' @details
#' The reverb creates the characteristic "spacious" sound of deep house pads
#' by simulating early reflections in a room. Each delay is progressively
#' quieter, creating a natural decay tail.
#'
#' @examples
#' \dontrun{
#' pad <- raver_pad_chord(48, "min9", 2.0)
#' padrev <- apply_pad_reverb(pad)
#' }
#'
#' @export
apply_pad_reverb <- function(wave, decay = 0.35, delays_ms = c(50, 100, 150, 200, 280, 380),
                              decay_rate = 0.55, wet_dry = 0.4) {
  if (!inherits(wave, "Wave")) {
    stop("wave must be a Wave object", call. = FALSE)
  }

  samples <- wave@left
  sample_rate <- wave@samp.rate
  n_samples <- length(samples)

  # Initialize wet signal (reverb only)
  wet <- numeric(n_samples)

  # Current decay level
  current_decay <- decay

  # Add each delayed reflection

  for (delay_ms in delays_ms) {
    delay_samples <- as.integer(delay_ms * sample_rate / 1000)

    if (delay_samples < n_samples) {
      # Create delayed copy
      delayed <- c(rep(0, delay_samples), samples[1:(n_samples - delay_samples)])
      # Add with current decay level
      wet <- wet + delayed * current_decay
      # Reduce decay for next reflection
      current_decay <- current_decay * decay_rate
    }
  }

  # Mix wet and dry signals
  output <- samples * (1 - wet_dry) + wet * wet_dry

  # Normalize to prevent clipping while preserving dynamics
  max_val <- max(abs(output))
  if (max_val > 0.95) {
    output <- output * (0.95 / max_val)
  }

  tuneR::Wave(
    left = output,
    samp.rate = sample_rate,
    bit = wave@bit,
    pcm = FALSE
  )
}

#' Generate a warm pad note
#'
#' @description Creates a warm pad tone using detuned oscillators for
#'   chorus-like richness. Uses a slow ADSR envelope for smooth attack.
#'
#' @param midi_note MIDI note number
#' @param duration_sec Duration in seconds
#' @param velocity Velocity from 0.0 to 1.0, scales amplitude. Default 0.7.
#' @param detune_cents Detuning amount in cents. Creates 3 oscillators:
#'   one at pitch, one +detune_cents, one -detune_cents. Default 3.
#'
#' @return A Wave object containing the pad note
#'
#' @details
#' The pad sound is created by layering multiple slightly detuned sine
#' oscillators, which produces a warm, chorus-like effect. The slow
#' ADSR envelope (attack=0.1, decay=0.2, sustain=0.7, release=0.3)
#' creates the characteristic pad "swell" effect.
#'
#' @examples
#' \dontrun{
#' # Single pad note
#' note <- raver_pad_note(60, 2.0)  # C4, 2 seconds
#'
#' # More detuned (wider chorus)
#' wide_note <- raver_pad_note(60, 2.0, detune_cents = 8)
#' }
#'
#' @export
raver_pad_note <- function(midi_note, duration_sec, velocity = 0.7, detune_cents = 3) {
  # Get base frequency
  base_freq <- raver_midi_to_freq(midi_note)

  # Calculate detuned frequencies
  # cents_to_ratio: 2^(cents/1200)
  detune_up <- 2^(detune_cents / 1200)
  detune_down <- 2^(-detune_cents / 1200)

  freq_center <- base_freq
  freq_up <- base_freq * detune_up
  freq_down <- base_freq * detune_down

  # Generate three sine oscillators
  osc_center <- raver_sine(freq_center, duration_sec, SAMPLE_RATE)
  osc_up <- raver_sine(freq_up, duration_sec, SAMPLE_RATE)
  osc_down <- raver_sine(freq_down, duration_sec, SAMPLE_RATE)

  # Mix oscillators (average to avoid clipping)
  mixed_samples <- (osc_center@left + osc_up@left + osc_down@left) / 3

  # Apply gentle lowpass filter for warmth (cutoff ~2000 Hz)
  # Using the same one-pole filter from bass.R
  cutoff_hz <- 2000
  alpha <- cutoff_hz / (cutoff_hz + SAMPLE_RATE / (2 * pi))

  n <- length(mixed_samples)
  filtered <- numeric(n)
  if (n > 0) {
    filtered[1] <- mixed_samples[1] * alpha
    for (i in 2:n) {
      filtered[i] <- (1 - alpha) * filtered[i - 1] + alpha * mixed_samples[i]
    }
  }

  # Apply slow ADSR envelope
  enveloped <- apply_adsr_pad(
    filtered,
    SAMPLE_RATE,
    attack = 0.1,
    decay = 0.2,
    sustain_level = 0.7,
    release = 0.3
  )

  # Scale by velocity
  enveloped <- enveloped * velocity

  tuneR::Wave(
    left = enveloped,
    samp.rate = SAMPLE_RATE,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Apply ADSR envelope (pad version)
#'
#' @description Internal helper for pad ADSR envelope. Same algorithm as
#'   bass, but defined locally to avoid dependency order issues.
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
apply_adsr_pad <- function(samples, sample_rate, attack, decay, sustain_level, release) {
  n_samples <- length(samples)

  # Convert times to sample counts
  attack_samples <- as.integer(attack * sample_rate)
  decay_samples <- as.integer(decay * sample_rate)
  release_samples <- as.integer(release * sample_rate)

  # Calculate sustain length
  envelope_samples <- attack_samples + decay_samples + release_samples
  if (n_samples > envelope_samples) {
    sustain_samples <- n_samples - envelope_samples
  } else {
    sustain_samples <- 0L
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

  # Sustain phase
  if (pos <= n_samples && sustain_samples > 0) {
    sustain_end <- min(pos + sustain_samples - 1, n_samples)
    envelope[pos:sustain_end] <- sustain_level
    pos <- sustain_end + 1
  }

  # Release phase
  if (pos <= n_samples) {
    release_length <- n_samples - pos + 1
    if (release_length > 0) {
      start_level <- if (pos > 1) envelope[pos - 1] else sustain_level
      envelope[pos:n_samples] <- seq(start_level, 0, length.out = release_length)
    }
  }

  samples * envelope
}

#' Generate a pad chord
#'
#' @description Creates a warm pad chord by combining multiple pad notes.
#'   Normalizes the output to prevent clipping from note stacking.
#'   Automatically applies reverb for that classic deep house spacious sound.
#'
#' @param root_midi MIDI note number for the chord root
#' @param chord_type Character name of chord type from CHORD_FORMULAS
#'   (e.g., "min7", "maj7", "min9"). Default "min9".
#' @param duration_sec Duration in seconds
#' @param velocity Velocity from 0.0 to 1.0. Default 0.85 (increased for audibility).
#' @param inversion Chord inversion (0 = root position, 1 = first inversion, etc.).
#'   Default 0.
#' @param reverb Logical, whether to apply reverb effect. Default TRUE.
#'
#' @return A Wave object containing the pad chord with reverb
#'
#' @details
#' The chord is built using raver_build_chord() to get MIDI notes,
#' then each note is rendered as a pad and mixed together. The mix
#' is normalized to prevent clipping from stacking multiple voices.
#' A master ADSR envelope is applied to the combined signal, and
#' reverb is added for the characteristic deep house pad sound.
#'
#' @examples
#' \dontrun{
#' # Cmin9 pad, 4 seconds
#' chord <- raver_pad_chord(48, "min9", 4.0)
#'
#' # Am7 in first inversion
#' chord_inv <- raver_pad_chord(57, "min7", 4.0, inversion = 1)
#' }
#'
#' @export
raver_pad_chord <- function(root_midi, chord_type = "min9", duration_sec,
                             velocity = 0.85, inversion = 0, reverb = TRUE) {
  # Build chord
  chord_notes <- raver_build_chord(root_midi, chord_type)

  # Apply inversion if requested
  if (inversion > 0) {
    chord_notes <- raver_invert_chord(chord_notes, inversion)
  }

  # Generate pad note for each chord tone (with full velocity, we'll scale later)
  pad_waves <- lapply(chord_notes, function(note) {
    raver_pad_note(note, duration_sec, velocity = 1.0)
  })

  # Mix all notes together
  n_notes <- length(pad_waves)
  n_samples <- length(pad_waves[[1]]@left)

  mixed <- numeric(n_samples)
  for (wave in pad_waves) {
    mixed <- mixed + wave@left
  }

  # Normalize to prevent clipping (divide by sqrt of notes for better loudness)
  mixed <- mixed / sqrt(n_notes)

  # Apply master ADSR to combined signal (smoother than individual envelopes)
  master_enveloped <- apply_adsr_pad(
    mixed,
    SAMPLE_RATE,
    attack = 0.15,
    decay = 0.2,
    sustain_level = 0.85,
    release = 0.4
  )

  # Scale by velocity
  master_enveloped <- master_enveloped * velocity

  result <- tuneR::Wave(
    left = master_enveloped,
    samp.rate = SAMPLE_RATE,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )

  # Apply reverb for spacious deep house sound
  if (reverb) {
    result <- apply_pad_reverb(result)
  }

  result
}

#' Create a pad chord progression
#'
#' @description Generates a sequence of pad chords forming a progression.
#'
#' @param chords List of lists, each containing:
#'   \itemize{
#'     \item root: MIDI note number for chord root
#'     \item type: Chord type string (e.g., "min7", "maj7")
#'   }
#' @param durations Vector of durations in bars. 1 = 1 bar, 0.5 = half bar.
#' @param bpm Beats per minute (tempo)
#' @param velocity Velocity for all chords. Default 0.6.
#'
#' @return A Wave object containing the full progression
#'
#' @examples
#' \dontrun{
#' # Am7 -> Fmaj7 progression, 1 bar each
#' prog <- list(
#'   list(root = 57, type = "min7"),
#'   list(root = 53, type = "maj7")
#' )
#' progression <- raver_pad_progression(prog, c(1, 1), bpm = 120)
#' }
#'
#' @export
raver_pad_progression <- function(chords, durations, bpm, velocity = 0.6) {
  if (length(chords) != length(durations)) {
    stop("chords and durations must have the same length")
  }

  if (length(chords) == 0) {
    return(create_silence(0, SAMPLE_RATE))
  }

  # Calculate bar duration in seconds
  bar_duration_sec <- (60 / bpm) * 4

  # Generate each chord
  chord_waves <- vector("list", length(chords))

  for (i in seq_along(chords)) {
    chord_info <- chords[[i]]
    duration_sec <- durations[i] * bar_duration_sec

    chord_waves[[i]] <- raver_pad_chord(
      root_midi = chord_info$root,
      chord_type = chord_info$type,
      duration_sec = duration_sec,
      velocity = velocity
    )
  }

  # Concatenate all chords
  do.call(bind_waves, chord_waves)
}
