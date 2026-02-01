#' @title Step Sequencer for Pattern-Based Composition
#' @description Step sequencer system for creating and rendering musical patterns.
#'   Handles precise timing, swing application, and combining multiple instrument
#'   patterns into cohesive bars for deep house production.
#' @name sequencer
NULL

# =============================================================================
# Pattern Creation
# =============================================================================

#' Create a pattern for the step sequencer
#'
#' @description Creates a pattern data structure for one bar of a single
#'   instrument. The pattern contains step triggers and timing information.
#'
#' @param instrument Character string identifying the instrument type.
#'   Valid values: "kick", "snare", "clap", "hihat_closed", "hihat_open",
#'   "bass", "pad"
#' @param steps A 16 or 32 element vector. Can be:
#'   - Logical: TRUE = hit at full velocity, FALSE = rest
#'   - Numeric: 0-1 velocity per step (0 = rest)
#' @param note MIDI note number for melodic instruments (bass, pad).
#'   Ignored for drums. Default NULL.
#' @param velocity Numeric base velocity multiplier from 0.0 to 1.0.
#'   Applied to all steps. Default 1.0.
#' @param swing Numeric swing amount from 0.0 to 0.25 for this pattern.
#'   Default 0.0.
#'
#' @return A pattern list with elements:
#'   \itemize{
#'     \item steps: Numeric vector of velocities (0-1) per step
#'     \item instrument: Character instrument identifier
#'     \item steps_per_bar: Integer (16 or 32)
#'     \item note: MIDI note or NULL
#'     \item velocity_base: Base velocity multiplier
#'     \item swing: Swing amount
#'   }
#'
#' @examples
#' \dontrun{
#' # Four-on-the-floor kick
#' kick <- create_pattern("kick",
#'   c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
#'     TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
#'
#' # Hi-hats with swing
#' hihat <- create_pattern("hihat_closed", rep(TRUE, 16), swing = 0.08)
#'
#' # Bass pattern with note
#' bass <- create_pattern("bass",
#'   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
#'     FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
#'   note = 36)  # C2
#' }
#'
#' @export
create_pattern <- function(instrument, steps, note = NULL, velocity = 1.0, swing = 0.0) {
  # Validate steps length
  if (!(length(steps) %in% c(16, 32))) {
    stop("steps must be a 16 or 32 element vector, got ", length(steps))
  }

  # Convert logical to numeric velocity
  if (is.logical(steps)) {
    steps <- as.numeric(steps)
  }

  # Ensure steps are in valid range
  steps <- pmax(0, pmin(1, as.numeric(steps)))

  # Clamp velocity and swing
  velocity <- max(0.0, min(1.0, velocity))
  swing <- max(0.0, min(0.25, swing))

  # Return pattern structure
  list(
    steps = steps,
    instrument = instrument,
    steps_per_bar = length(steps),
    note = note,
    velocity_base = velocity,
    swing = swing
  )
}

# =============================================================================
# Internal Helpers
# =============================================================================

#' Render a single instrument hit
#'
#' @description Internal helper that dispatches to the appropriate instrument
#'   function based on instrument type.
#'
#' @param instrument Character instrument type
#' @param note MIDI note (for melodic instruments)
#' @param velocity Velocity 0-1
#' @param duration_sec Duration in seconds
#' @param chord_type Chord type for pad instrument (default "min9")
#'
#' @return Wave object or NULL if instrument not available
#' @keywords internal
render_step <- function(instrument, note, velocity, duration_sec, chord_type = "min9") {
  result <- switch(instrument,
    kick = raver_drum_hit("kick", velocity, duration_sec),
    snare = raver_drum_hit("snare", velocity, duration_sec),
    clap = raver_drum_hit("clap", velocity, duration_sec),
    hihat_closed = raver_drum_hit("hihat_closed", velocity, duration_sec),
    hihat_open = raver_drum_hit("hihat_open", velocity, duration_sec),
    bass = raver_bass_note(note, duration_sec, velocity),
    pad = raver_pad_chord(note, chord_type, duration_sec, velocity),
    {
      warning("Unknown instrument: ", instrument, call. = FALSE)
      NULL
    }
  )
  result
}

#' Mix waves at specific sample positions
#'
#' @description Internal helper for mixing multiple Wave objects at specific
#'   sample positions within a base Wave.
#'
#' @param base_wave Wave object to mix into (typically silence)
#' @param waves List of Wave objects to mix
#' @param positions Numeric vector of sample positions (same length as waves)
#'
#' @return Modified Wave object with all waves mixed in
#' @keywords internal
mix_waves_at_positions <- function(base_wave, waves, positions) {
  if (length(waves) != length(positions)) {
    stop("waves and positions must have the same length")
  }

  output <- base_wave@left
  n_output <- length(output)

  for (i in seq_along(waves)) {
    wave <- waves[[i]]
    if (is.null(wave)) next

    pos <- as.integer(positions[i]) + 1L  # Convert to 1-indexed
    if (pos < 1) pos <- 1L

    wave_samples <- wave@left
    n_wave <- length(wave_samples)

    # Calculate how much fits in the buffer
    available_space <- n_output - pos + 1
    samples_to_mix <- min(n_wave, available_space)

    if (samples_to_mix > 0) {
      end_pos <- pos + samples_to_mix - 1
      output[pos:end_pos] <- output[pos:end_pos] + wave_samples[seq_len(samples_to_mix)]
    }
  }

  tuneR::Wave(
    left = output,
    samp.rate = base_wave@samp.rate,
    bit = base_wave@bit,
    pcm = FALSE
  )
}

# =============================================================================
# Pattern Rendering
# =============================================================================

#' Render a pattern to audio
#'
#' @description Renders a pattern to a Wave object representing one bar of audio.
#'   Applies swing timing and velocity scaling to each step.
#'
#' @param pattern A pattern list created by create_pattern()
#' @param bpm Numeric beats per minute (tempo)
#' @param sample_rate Integer sample rate in Hz. Default SAMPLE_RATE.
#'
#' @return A Wave object containing one bar of the rendered pattern
#'
#' @details
#' The pattern is rendered step by step:
#' 1. Calculate the sample position for each step (with swing applied)
#' 2. For each active step (velocity > 0), render the instrument hit
#' 3. Mix all hits into the output buffer
#' 4. Overlapping notes are summed (mixed), not replaced
#'
#' @examples
#' \dontrun{
#' # Create and render a kick pattern
#' kick_pat <- create_pattern("kick",
#'   c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
#'     TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
#' kick_wave <- render_pattern(kick_pat, bpm = 120)
#' }
#'
#' @export
render_pattern <- function(pattern, bpm, sample_rate = SAMPLE_RATE) {
  # Calculate bar duration
  bar_duration_sec <- (60 / bpm) * 4
  bar_samples <- as.integer(bar_duration_sec * sample_rate)

  # Initialize output buffer
  output <- numeric(bar_samples)

  # Extract pattern properties
  steps <- pattern$steps
  n_steps <- pattern$steps_per_bar
  instrument <- pattern$instrument
  note <- pattern$note
  base_velocity <- pattern$velocity_base
  swing <- pattern$swing

  # Step duration for note length calculation
  step_duration_sec <- bar_duration_sec / n_steps

  # Pads need longer sustain (full bar duration) to sound like chords
  # rather than short blips. The ADSR attack alone is 0.15s, longer than
  # a single step (0.125s at 120bpm), so short durations cut off the chord.
  pad_duration_sec <- bar_duration_sec

  # Collect waves and positions for active steps
  waves <- list()
  positions <- integer(0)

  for (step_idx in seq_len(n_steps)) {
    step_velocity <- steps[step_idx]

    if (step_velocity > 0) {
      # Calculate position with swing
      timing <- calculate_step_timing(step_idx, bpm, swing, n_steps)
      position <- timing$position_samples

      # Calculate effective velocity
      effective_velocity <- step_velocity * base_velocity

      # Use appropriate duration for instrument type
      # Pads sustain for full bar, other instruments use step duration
      note_duration <- if (instrument == "pad") pad_duration_sec else step_duration_sec

      # Render the instrument hit
      wave <- render_step(instrument, note, effective_velocity, note_duration)

      if (!is.null(wave)) {
        waves <- c(waves, list(wave))
        positions <- c(positions, position)
      }
    }
  }

  # Mix all waves at their positions
  if (length(waves) > 0) {
    base_wave <- tuneR::Wave(
      left = output,
      samp.rate = sample_rate,
      bit = BIT_DEPTH,
      pcm = PCM_MODE
    )
    result <- mix_waves_at_positions(base_wave, waves, positions)
    return(result)
  }

  # Return silence if no active steps
  tuneR::Wave(
    left = output,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Combine multiple patterns into one bar
#'
#' @description Renders multiple patterns and mixes them together into a single
#'   Wave object. Normalizes the output to prevent clipping.
#'
#' @param patterns List of pattern objects created by create_pattern()
#' @param bpm Numeric beats per minute (tempo)
#' @param sample_rate Integer sample rate in Hz. Default SAMPLE_RATE.
#'
#' @return A Wave object containing all patterns mixed together
#'
#' @details
#' Each pattern is rendered independently, then all rendered patterns are
#' mixed together. The mix is normalized by dividing by sqrt(n_patterns)
#' to prevent clipping while maintaining relative loudness.
#'
#' @examples
#' \dontrun{
#' # Create drum patterns
#' kick <- create_pattern("kick",
#'   c(T,F,F,F, T,F,F,F, T,F,F,F, T,F,F,F))
#' snare <- create_pattern("snare",
#'   c(F,F,F,F, T,F,F,F, F,F,F,F, T,F,F,F))
#' hihat <- create_pattern("hihat_closed", rep(TRUE, 16), swing = 0.08)
#'
#' # Combine into groove
#' groove <- combine_patterns(list(kick, snare, hihat), bpm = 120)
#' }
#'
#' @export
combine_patterns <- function(patterns, bpm, sample_rate = SAMPLE_RATE) {
  if (length(patterns) == 0) {
    bar_duration_sec <- (60 / bpm) * 4
    return(create_silence(bar_duration_sec, sample_rate))
  }

  # Render each pattern
  rendered <- lapply(patterns, function(pat) {
    render_pattern(pat, bpm, sample_rate)
  })

  # Get expected bar length
  bar_duration_sec <- (60 / bpm) * 4
  bar_samples <- as.integer(bar_duration_sec * sample_rate)

  # Mix all rendered patterns
  n_patterns <- length(rendered)
  mixed <- numeric(bar_samples)

  for (wave in rendered) {
    wave_samples <- wave@left
    n_wave <- length(wave_samples)
    n_to_mix <- min(n_wave, bar_samples)

    if (n_to_mix > 0) {
      mixed[seq_len(n_to_mix)] <- mixed[seq_len(n_to_mix)] + wave_samples[seq_len(n_to_mix)]
    }
  }

  # Normalize to prevent clipping
  # Using sqrt(n) maintains relative loudness better than dividing by n
  mixed <- mixed / sqrt(n_patterns)

  # Soft clip any values still above 1.0
  max_val <- max(abs(mixed))
  if (max_val > 1.0) {
    mixed <- mixed / max_val
  }

  tuneR::Wave(
    left = mixed,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}
