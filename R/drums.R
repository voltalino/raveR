#' @title Drum Instrument with Sample Triggering
#' @description 909-style drum instrument for deep house production.
#'   Supports sample triggering with velocity scaling and pattern-based
#'   sequencing with swing for authentic house groove.
#' @name drums
NULL

# Package environment for drum kit cache
.drum_env <- new.env(parent = emptyenv())

#' Get cached drum kit
#'
#' Internal function that loads and caches the drum kit.
#'
#' @return Named list of drum samples
#' @keywords internal
get_drum_kit_cached <- function() {
  if (exists("kit", envir = .drum_env)) {
    return(get("kit", envir = .drum_env))
  }

  kit <- raver_get_drum_kit()
  assign("kit", kit, envir = .drum_env)
  kit
}

#' Trigger a drum hit with velocity scaling
#'
#' @description Triggers a drum sample with velocity-based amplitude scaling.
#'   Returns a Wave object that can be mixed with other instruments.
#'
#' @param drum_type Character specifying the drum type: "kick", "snare",
#'   "clap", "hihat_closed", or "hihat_open"
#' @param velocity Numeric velocity from 0.0 to 1.0, scales amplitude.
#'   Default 1.0 (full velocity).
#' @param duration_sec Numeric duration in seconds. If NULL, uses full sample
#'   length. If shorter than sample, truncates. If longer, pads with silence.
#'
#' @return A Wave object, or NULL if the sample is missing
#'
#' @details The velocity parameter scales the sample amplitude linearly.
#'   A velocity of 0.5 produces half the amplitude, which is approximately
#'   -6 dB. Missing samples return NULL with a warning rather than
#'   throwing an error to allow graceful degradation.
#'
#' @examples
#' \dontrun{
#' kick <- raver_drum_hit("kick", velocity = 0.8)
#' snare <- raver_drum_hit("snare", velocity = 0.9)
#' soft_hihat <- raver_drum_hit("hihat_closed", velocity = 0.4)
#' }
#'
#' @export
raver_drum_hit <- function(drum_type, velocity = 1.0, duration_sec = NULL) {
  # Validate drum type
  valid_types <- c("kick", "snare", "clap", "hihat_closed", "hihat_open")
  if (!(drum_type %in% valid_types)) {
    stop("Invalid drum_type: ", drum_type,
         ". Must be one of: ", paste(valid_types, collapse = ", "))
  }

  # Clamp velocity to valid range
  velocity <- max(0.0, min(1.0, velocity))

  # Get sample from cached kit
  kit <- get_drum_kit_cached()
  sample <- kit[[drum_type]]

  # Handle missing sample gracefully
  if (is.null(sample)) {
    warning("Missing drum sample: ", drum_type, call. = FALSE)
    return(NULL)
  }

  # Scale by velocity
  scaled_samples <- sample@left * velocity

  # Handle duration
  if (!is.null(duration_sec)) {
    target_length <- as.integer(duration_sec * sample@samp.rate)
    current_length <- length(scaled_samples)

    if (target_length < current_length) {
      # Truncate
      scaled_samples <- scaled_samples[seq_len(target_length)]
    } else if (target_length > current_length) {
      # Pad with zeros (silence)
      scaled_samples <- c(scaled_samples,
                          numeric(target_length - current_length))
    }
  }

  # Create output Wave
  tuneR::Wave(
    left = scaled_samples,
    samp.rate = SAMPLE_RATE,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Calculate swing offset for a step
#'
#' @description Internal helper that calculates the sample offset for swing.
#'   Even-numbered steps (2, 4, 6, ...) are delayed according to the
#'   Roger Linn swing algorithm.
#'
#' @param step Integer step number (1-based)
#' @param swing_amount Numeric swing amount from 0.0 to 0.25
#' @param samples_per_16th Integer samples per 16th note
#'
#' @return Integer sample offset to apply
#' @keywords internal
calculate_swing_offset <- function(step, swing_amount, samples_per_16th) {
  # Even steps get delayed (1-indexed, so 2, 4, 6... are even)
  if (step %% 2 == 0) {
    return(as.integer(round(swing_amount * samples_per_16th)))
  }
  0L
}

#' Create a drum pattern for one bar
#'
#' @description Creates a 16-step drum pattern for one bar at the specified
#'   tempo with optional swing. Returns a Wave object containing all hits
#'   placed on a 16th-note grid.
#'
#' @param pattern Logical vector of 16 elements. TRUE triggers a hit on that
#'   16th note step.
#' @param bpm Numeric beats per minute (tempo)
#' @param drum_type Character drum type to use for hits
#' @param swing Numeric swing amount from 0.0 to 0.25. Delays even-numbered
#'   steps for groove. Default 0.0 (no swing).
#'
#' @return A Wave object containing one bar with the drum pattern
#'
#' @details
#' The pattern is a 16-element logical vector representing 16th notes in
#' one bar. For example, a 4-on-the-floor kick pattern would be:
#' `c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, ...)`
#'
#' Swing delays even-numbered steps (2nd, 4th, 6th, etc.) by a fraction
#' of a 16th note. Common house swing values are 0.05 to 0.15.
#'
#' @examples
#' \dontrun{
#' # 4-on-the-floor kick pattern
#' four_on_floor <- c(TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 3),
#'                    TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 3))
#' kick_bar <- raver_drum_pattern(four_on_floor, bpm = 120, drum_type = "kick")
#'
#' # Offbeat hi-hats with swing
#' offbeat <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE,
#'              FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
#' hihat_bar <- raver_drum_pattern(offbeat, bpm = 120,
#'                                  drum_type = "hihat_closed", swing = 0.1)
#' }
#'
#' @export
raver_drum_pattern <- function(pattern, bpm, drum_type = "kick", swing = 0.0) {
  # Validate pattern
  if (length(pattern) != 16) {
    stop("Pattern must be a 16-element logical vector")
  }
  pattern <- as.logical(pattern)

  # Clamp swing to valid range
  swing <- max(0.0, min(0.25, swing))

  # Calculate timing
  # One bar = 4 beats at this BPM
  bar_duration_sec <- (60 / bpm) * 4
  bar_samples <- as.integer(bar_duration_sec * SAMPLE_RATE)

  # 16th note duration
  samples_per_16th <- bar_samples / 16  # Keep as float for accuracy

  # Initialize output buffer
  output <- numeric(bar_samples)

  # Get drum sample once
  hit <- raver_drum_hit(drum_type, velocity = 1.0)

  if (is.null(hit)) {
    # Return silence if sample is missing
    warning("Cannot create pattern: ", drum_type, " sample is missing",
            call. = FALSE)
    return(tuneR::Wave(
      left = output,
      samp.rate = SAMPLE_RATE,
      bit = BIT_DEPTH,
      pcm = PCM_MODE
    ))
  }

  hit_samples <- hit@left
  hit_length <- length(hit_samples)

  # Place each hit at the correct position
  for (step in seq_along(pattern)) {
    if (pattern[step]) {
      # Calculate position using floating-point, convert to integer only at end
      base_position <- (step - 1) * samples_per_16th
      swing_offset <- calculate_swing_offset(step, swing, samples_per_16th)
      position <- as.integer(round(base_position + swing_offset)) + 1L  # 1-indexed

      # Calculate how much of the hit fits in the buffer
      available_space <- bar_samples - position + 1
      samples_to_copy <- min(hit_length, available_space)

      if (samples_to_copy > 0) {
        # Mix (add) the hit at this position
        end_pos <- position + samples_to_copy - 1
        output[position:end_pos] <- output[position:end_pos] +
                                     hit_samples[seq_len(samples_to_copy)]
      }
    }
  }

  # Create output Wave
  tuneR::Wave(
    left = output,
    samp.rate = SAMPLE_RATE,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}
