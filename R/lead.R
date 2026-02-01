#' @title Lead Synthesizer for Melodic Lines
#' @description Bright lead synthesizer for deep house melodies.
#'   Creates cutting, melodic lead sounds using detuned oscillators
#'   with filter modulation for movement.
#' @name lead
NULL

#' Generate a lead synth note
#'
#' @description Creates a bright lead note using layered oscillators.
#'   Combines sawtooth and square waves for a rich, cutting sound.
#'
#' @param midi_note MIDI note number
#' @param duration_sec Duration in seconds
#' @param velocity Velocity from 0.0 to 1.0. Default 0.8.
#' @param detune_cents Detuning for chorusing effect. Default 5.
#'
#' @return A Wave object containing the lead note
#'
#' @details
#' The lead uses a mix of sawtooth and square waves, slightly detuned
#' for richness. A moderate lowpass filter keeps it from being too harsh
#' while maintaining presence in the mix.
#'
#' @examples
#' \dontrun{
#' note <- raver_lead_note(60, 0.5)  # C4, half second
#' }
#'
#' @export
raver_lead_note <- function(midi_note, duration_sec, velocity = 0.8, detune_cents = 5) {
  freq <- raver_midi_to_freq(midi_note)

  # Detune factors
  detune_up <- 2^(detune_cents / 1200)
  detune_down <- 2^(-detune_cents / 1200)

  # Generate oscillators - mix saw and square
  saw_center <- raver_sawtooth(freq, duration_sec, SAMPLE_RATE)
  saw_up <- raver_sawtooth(freq * detune_up, duration_sec, SAMPLE_RATE)
  square_down <- raver_square(freq * detune_down, duration_sec, SAMPLE_RATE, duty = 0.4)

  # Mix oscillators (saw dominant, square for body)
  mixed <- (saw_center@left * 0.5 + saw_up@left * 0.25 + square_down@left * 0.25)

  # Apply lowpass filter for warmth (higher than bass, lower than raw)
  cutoff_hz <- 3000
  alpha <- cutoff_hz / (cutoff_hz + SAMPLE_RATE / (2 * pi))

  n <- length(mixed)
  filtered <- numeric(n)
  if (n > 0) {
    filtered[1] <- mixed[1] * alpha
    for (i in 2:n) {
      filtered[i] <- (1 - alpha) * filtered[i - 1] + alpha * mixed[i]
    }
  }

  # Apply snappy ADSR envelope
  enveloped <- apply_adsr(
    filtered,
    SAMPLE_RATE,
    attack = 0.01,
    decay = 0.15,
    sustain_level = 0.6,
    release = 0.15
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

#' Generate a percussive pluck lead note
#'
#' @description Creates a short, percussive pluck sound for rhythmic leads.
#'   Uses a faster envelope and brighter tone than the standard lead.
#'
#' @param midi_note MIDI note number
#' @param duration_sec Duration in seconds
#' @param velocity Velocity from 0.0 to 1.0. Default 0.8.
#'
#' @return A Wave object containing the percussive note
#'
#' @export
raver_pluck_note <- function(midi_note, duration_sec, velocity = 0.8) {
  freq <- raver_midi_to_freq(midi_note)

  # Generate oscillators - square wave dominant for plucky sound
  square_main <- raver_square(freq, duration_sec, SAMPLE_RATE, duty = 0.25)
  saw_sub <- raver_sawtooth(freq * 0.5, duration_sec, SAMPLE_RATE)  # Sub octave

  # Mix oscillators (square dominant, saw for body)
  mixed <- (square_main@left * 0.7 + saw_sub@left * 0.3)

  # Higher cutoff for brighter attack, with envelope
  n <- length(mixed)
  filtered <- numeric(n)
  if (n > 0) {
    # Filter envelope: starts bright, darkens quickly
    for (i in seq_len(n)) {
      progress <- i / n
      cutoff_hz <- 5000 * exp(-progress * 3) + 800  # Decays from 5800 to 800
      alpha <- cutoff_hz / (cutoff_hz + SAMPLE_RATE / (2 * pi))
      if (i == 1) {
        filtered[i] <- mixed[i] * alpha
      } else {
        filtered[i] <- (1 - alpha) * filtered[i - 1] + alpha * mixed[i]
      }
    }
  }

  # Percussive ADSR: very fast attack, quick decay, low sustain
  enveloped <- apply_adsr(
    filtered,
    SAMPLE_RATE,
    attack = 0.002,   # 2ms attack
    decay = 0.08,     # 80ms decay
    sustain_level = 0.2,
    release = 0.1
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

#' Apply rhythmic delay to audio
#'
#' @description Adds a rhythmic delay effect with feedback. Common for house
#'   music leads - creates movement and fills space.
#'
#' @param samples Numeric vector of audio samples
#' @param delay_sec Delay time in seconds
#' @param feedback Feedback amount (0-0.9). Higher = more repeats. Default 0.4.
#' @param mix Wet/dry mix (0-1). Default 0.5.
#' @param sample_rate Sample rate. Default SAMPLE_RATE.
#'
#' @return Numeric vector with delay applied
#'
#' @export
apply_rhythmic_delay <- function(samples, delay_sec, feedback = 0.4,
                                  mix = 0.5, sample_rate = SAMPLE_RATE) {
  n <- length(samples)
  delay_samples <- as.integer(delay_sec * sample_rate)

  if (delay_samples <= 0 || delay_samples >= n) {
    return(samples)
  }

  # Clamp feedback to prevent runaway

  feedback <- min(0.9, max(0, feedback))

  # Create delay buffer with extra space for tails
  output <- numeric(n + delay_samples * 4)
  output[seq_len(n)] <- samples

  # Apply feedback delay
  for (i in (delay_samples + 1):length(output)) {
    delayed_idx <- i - delay_samples
    if (delayed_idx > 0 && delayed_idx <= length(output)) {
      output[i] <- output[i] + output[delayed_idx] * feedback
    }
  }

  # Mix wet/dry and trim to original length
  wet <- output[seq_len(n)]
  result <- samples * (1 - mix) + wet * mix

  result
}

#' Generate a lead melody from a motif
#'
#' @description Creates a lead melody line from a motif pattern.
#'   Each active step in the motif triggers a lead note.
#'
#' @param motif A motif from generate_motif()
#' @param bpm Beats per minute
#' @param bars Number of bars to generate. Default 1.
#' @param velocity Base velocity. Default 0.7.
#'
#' @return A Wave object containing the lead melody
#'
#' @export
raver_lead_melody <- function(motif, bpm, bars = 1L, velocity = 0.7) {
  # Calculate timing

  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / 16
  total_samples <- as.integer(bar_duration_sec * bars * SAMPLE_RATE)

  # Initialize output
  output <- numeric(total_samples)

  # Get active steps and their notes
  active_steps <- which(motif$rhythm)

  for (bar in seq_len(bars)) {
    bar_offset <- (bar - 1) * 16

    for (i in seq_along(active_steps)) {
      step_idx <- active_steps[i]
      global_step <- bar_offset + step_idx

      # Get note and velocity for this step
      note_idx <- ((i - 1) %% length(motif$notes)) + 1
      vel_idx <- ((i - 1) %% length(motif$velocities)) + 1

      note <- motif$notes[note_idx]
      step_vel <- motif$velocities[vel_idx] * velocity

      # Calculate position
      position <- as.integer((global_step - 1) * step_duration_sec * SAMPLE_RATE) + 1

      # Generate lead note (duration = 2 steps for legato feel)
      note_duration <- step_duration_sec * 2
      lead_wave <- raver_lead_note(note, note_duration, step_vel)

      # Mix into output
      wave_samples <- lead_wave@left
      n_wave <- length(wave_samples)
      end_pos <- min(position + n_wave - 1, total_samples)

      if (position <= total_samples && end_pos >= position) {
        n_to_mix <- end_pos - position + 1
        output[position:end_pos] <- output[position:end_pos] + wave_samples[seq_len(n_to_mix)]
      }
    }
  }

  tuneR::Wave(
    left = output,
    samp.rate = SAMPLE_RATE,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}
