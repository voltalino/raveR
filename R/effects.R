#' @title Effects Depth from Call Stack
#' @description Maps code complexity and nesting depth to audio effects
#'   (reverb, delay) for spatial and textural variation.
#' @name effects
NULL

# =============================================================================
# Effects Configuration
# =============================================================================

#' Calculate Reverb Parameters from Code Depth
#'
#' @description Maps nesting depth and cyclomatic complexity to reverb
#'   room size and decay time. Deep recursion = cavernous reverb.
#'
#' @param nesting_depth Integer maximum nesting depth
#' @param cyclomatic_complexity Integer cyclomatic complexity
#' @param genre Character genre for context
#'
#' @return List with room_size (0-1), decay_time (seconds), wet_level (0-1)
#' @keywords internal
calculate_reverb_params <- function(nesting_depth, cyclomatic_complexity, genre = "deep_house") {
  # Base values by genre
  genre_bases <- list(
    deep_house = list(room_size = 0.4, decay = 1.5, wet = 0.25),
    techno = list(room_size = 0.3, decay = 0.8, wet = 0.15),
    ambient = list(room_size = 0.8, decay = 4.0, wet = 0.6),
    drum_bass = list(room_size = 0.2, decay = 0.5, wet = 0.1)
  )

  base <- genre_bases[[genre]]

  # Depth increases room size (0.1 per level, capped)
  depth_factor <- min(nesting_depth * 0.1, 0.5)

  # Complexity increases decay (0.2 per complexity point, capped)
  complexity_factor <- min(cyclomatic_complexity * 0.05, 1.0)

  list(
    room_size = min(base$room_size + depth_factor, 1.0),
    decay_time = base$decay * (1 + complexity_factor),
    wet_level = min(base$wet + (depth_factor * 0.3), 0.8)
  )
}

#' Calculate Delay Parameters from Control Flow
#'
#' @description Maps control flow density to delay time and feedback.
#'   More control flow = more rhythmic delay taps.
#'
#' @param control_flow_count Integer count of control flow statements
#' @param bpm Numeric tempo
#' @param genre Character genre
#'
#' @return List with delay_time (seconds), feedback (0-1), taps (integer)
#' @keywords internal
calculate_delay_params <- function(control_flow_count, bpm, genre = "deep_house") {
  beat_duration <- 60 / bpm

  # Base delay times by genre (in beats)
  genre_delays <- list(
    deep_house = c(0.5, 0.75),      # Eighth and dotted eighth
    techno = c(0.25, 0.5),          # Sixteenth and eighth
    ambient = c(1.0, 2.0),          # Whole and half note
    drum_bass = c(0.333, 0.666)     # Triplet feel
  )

  base_delays <- genre_delays[[genre]]

  # More control flow = more taps (up to 4)
  taps <- min(control_flow_count %/% 3 + 1, 4)

  # Select delay time based on control flow count
  delay_idx <- (control_flow_count %% length(base_delays)) + 1
  delay_beats <- base_delays[delay_idx]

  # Feedback increases with complexity (0.2 to 0.6)
  feedback <- min(0.2 + (control_flow_count * 0.05), 0.6)

  list(
    delay_time = delay_beats * beat_duration,
    feedback = feedback,
    taps = taps
  )
}

#' Apply Reverb to Audio Buffer
#'
#' @description Simple Schroeder reverb implementation for R.
#'   Applies calculated reverb parameters to a Wave object.
#'
#' @param wave tuneR Wave object
#' @param reverb_params List from calculate_reverb_params()
#'
#' @return Wave object with reverb applied
#' @keywords internal
apply_reverb <- function(wave, reverb_params) {
  samples <- wave@left
  sr <- wave@samp.rate

  # Simple comb filter reverb (Schroeder-style)
  decay_samples <- round(reverb_params$decay_time * sr)
  room_scale <- reverb_params$room_size

  # Create comb filters with different delays
  comb_delays <- round(c(0.029, 0.037, 0.041, 0.043) * sr * (0.5 + room_scale))
  comb_gains <- reverb_params$wet_level * c(0.8, 0.7, 0.6, 0.5)

  # Apply comb filters
  reverb_signal <- numeric(length(samples))
  for (i in seq_along(comb_delays)) {
    delay <- comb_delays[i]
    gain <- comb_gains[i]

    if (delay > 0 && delay < length(samples)) {
      echo <- numeric(length(samples))
      echo[(delay + 1):length(samples)] <- samples[1:(length(samples) - delay)]
      reverb_signal <- reverb_signal + (echo * gain)
    }
  }

  # Mix dry and wet
  dry_gain <- 1 - (reverb_params$wet_level * 0.5)
  wet_gain <- reverb_params$wet_level

  mixed <- (samples * dry_gain) + (reverb_signal * wet_gain)

  # Normalize to prevent clipping
  max_val <- max(abs(mixed))
  if (max_val > 0) {
    mixed <- mixed / max_val * 0.95
  }

  tuneR::Wave(
    left = mixed,
    samp.rate = sr,
    bit = wave@bit,
    pcm = FALSE
  )
}

#' Apply Delay to Audio Buffer
#'
#' @description Simple delay line with feedback.
#'
#' @param wave tuneR Wave object
#' @param delay_params List from calculate_delay_params()
#'
#' @return Wave object with delay applied
#' @keywords internal
apply_delay <- function(wave, delay_params) {
  samples <- wave@left
  sr <- wave@samp.rate

  delay_samples <- round(delay_params$delay_time * sr)
  feedback <- delay_params$feedback

  if (delay_samples <= 0 || delay_samples >= length(samples)) {
    return(wave)
  }

  # Create delay buffer
  output <- samples
  delay_buffer <- numeric(length(samples))

  for (i in (delay_samples + 1):length(samples)) {
    delay_sample <- delay_buffer[i - delay_samples]
    delay_buffer[i] <- samples[i] + (delay_sample * feedback)
    output[i] <- samples[i] + delay_sample
  }

  # Normalize
  max_val <- max(abs(output))
  if (max_val > 0) {
    output <- output / max_val * 0.95
  }

  tuneR::Wave(
    left = output,
    samp.rate = sr,
    bit = wave@bit,
    pcm = FALSE
  )
}

#' Apply Effects Based on Code Complexity
#'
#' @description Main entry point for applying code-driven effects to
#'   a generated audio section.
#'
#' @param wave tuneR Wave object
#' @param code_model CodeModel object
#' @param genre Character genre
#'
#' @return Wave object with effects applied
#' @export
apply_code_effects <- function(wave, code_model, genre = "deep_house") {
  # Calculate effect parameters from code metrics
  reverb_params <- calculate_reverb_params(
    code_model$nesting_depth,
    code_model$cyclomatic_complexity,
    genre
  )

  delay_params <- calculate_delay_params(
    code_model$control_flow_count,
    120,  # Default BPM for calculation
    genre
  )

  # Apply effects chain
  wave <- apply_reverb(wave, reverb_params)
  wave <- apply_delay(wave, delay_params)

  wave
}
