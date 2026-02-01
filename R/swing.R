#' @title Swing and Groove Timing
#' @description Swing timing utilities for authentic deep house groove.
#'   Implements the Roger Linn swing algorithm where even-numbered steps
#'   are delayed to create the characteristic house feel.
#' @name swing
NULL

# =============================================================================
# Groove Presets
# =============================================================================

#' Groove presets for common swing amounts
#'
#' @description Named list of groove presets with swing amounts and descriptions.
#'   Swing values represent the fraction of a 16th note that even steps are delayed.
#'
#' @details
#' The swing algorithm delays even-numbered steps (2, 4, 6, ...) while keeping
#' odd-numbered steps on the grid. This creates the characteristic "shuffle" feel
#' of house music.
#'
#' Common values:
#' - 0.0: Straight, machine-like timing
#' - 0.05: Subtle human feel
#' - 0.10: Classic house groove
#' - 0.15: Strong triplet feel
#' - 0.167: MPC-style triplet swing (exactly 1/6)
#'
#' @export
GROOVE_PRESETS <- list(
  straight = list(swing = 0.0, description = "No swing, machine-like"),
  light = list(swing = 0.05, description = "Subtle human feel"),
  medium = list(swing = 0.10, description = "Classic house groove"),
  heavy = list(swing = 0.15, description = "Strong triplet feel"),
  mpc = list(swing = 0.167, description = "MPC-style triplet swing")
)

# =============================================================================
# Timing Calculation Functions
# =============================================================================

#' Calculate exact timing for a step position
#'
#' @description Calculates the exact sample and time position for a step in a
#'   pattern, applying swing to even-numbered steps using the Roger Linn algorithm.
#'
#' @param step Integer step number (1-indexed: 1, 2, 3, ... up to steps_per_bar)
#' @param bpm Numeric beats per minute (tempo)
#' @param swing_amount Numeric swing amount from 0.0 to 0.25. Even steps are
#'   delayed by this fraction of a step duration. Default 0.0.
#' @param steps_per_bar Integer steps per bar. Default 16 (16th notes).
#'
#' @return List with:
#'   \itemize{
#'     \item position_samples: Integer sample position from bar start
#'     \item position_sec: Numeric seconds from bar start
#'   }
#'
#' @details
#' The Roger Linn swing algorithm delays even-numbered steps while keeping
#' odd-numbered steps on the grid. The delay amount is swing_amount multiplied
#' by the step duration.
#'
#' For example, with swing_amount = 0.10 at 120 BPM:
#' - Step 1: 0 samples (on grid)
#' - Step 2: base position + 10% of step duration
#' - Step 3: base position (on grid)
#' - Step 4: base position + 10% of step duration
#'
#' @examples
#' \dontrun{
#' # Step 1 timing at 120 BPM
#' t1 <- calculate_step_timing(1, 120)
#' # Step 2 with 10% swing
#' t2 <- calculate_step_timing(2, 120, swing_amount = 0.10)
#' }
#'
#' @export
calculate_step_timing <- function(step, bpm, swing_amount = 0.0, steps_per_bar = 16) {
  # Calculate bar duration
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / steps_per_bar

  # Base position for this step (0-indexed calculation)
  base_position_sec <- (step - 1) * step_duration_sec

  # Apply swing to even steps (step 2, 4, 6, ...)
  swing_offset_sec <- 0.0
  if (step %% 2 == 0) {
    swing_offset_sec <- swing_amount * step_duration_sec
  }

  position_sec <- base_position_sec + swing_offset_sec
  position_samples <- as.integer(round(position_sec * SAMPLE_RATE))

  list(
    position_samples = position_samples,
    position_sec = position_sec
  )
}

#' Apply swing to step positions
#'
#' @description Adjusts a vector of step positions by applying swing to
#'   even-numbered steps. The returned positions may be fractional.
#'
#' @param steps Numeric vector of step positions (1-indexed: 1, 2, 3, ...)
#' @param swing_amount Numeric swing amount from 0.0 to 0.25
#'
#' @return Numeric vector of adjusted step positions (may be fractional)
#'
#' @details
#' This function is useful for converting step patterns to precise timing
#' before rendering. Even steps are delayed by swing_amount (as a fraction
#' of one step). Odd steps remain on grid.
#'
#' @examples
#' \dontrun{
#' # Apply 10% swing to steps 1-4
#' swung <- apply_swing_to_steps(1:4, 0.10)
#' # swung = c(1.0, 2.1, 3.0, 4.1)
#' }
#'
#' @export
apply_swing_to_steps <- function(steps, swing_amount) {
  # Apply swing offset to even steps
  adjusted <- sapply(steps, function(s) {
    if (s %% 2 == 0) {
      s + swing_amount
    } else {
      s
    }
  })

  as.numeric(adjusted)
}

#' Add humanization to timing
#'
#' @description Adds subtle random variation to sample position for a more
#'   human feel. The variation is constrained to avoid noticeable timing errors.
#'
#' @param position_samples Integer sample position to humanize
#' @param amount Numeric maximum deviation as fraction of a 16th note at 120 BPM.
#'   Default 0.02 (2% of a 16th note, approximately 0.5ms).
#' @param seed Optional integer seed for deterministic random (reproducibility).
#'   Default NULL uses non-deterministic random.
#'
#' @return Integer adjusted sample position
#'
#' @details
#' Humanization adds very small random offsets to make patterns feel less
#' robotic. The offset is uniformly distributed between -amount and +amount
#' of a 16th note duration (at reference 120 BPM).
#'
#' @examples
#' \dontrun{
#' # Humanize with deterministic seed
#' pos <- humanize_timing(44100, amount = 0.02, seed = 42)
#' }
#'
#' @export
humanize_timing <- function(position_samples, amount = 0.02, seed = NULL) {
  # Reference 16th note duration at 120 BPM
  # At 120 BPM: bar = 2 sec, 16th = 0.125 sec
  ref_16th_samples <- as.integer(0.125 * SAMPLE_RATE)

  # Maximum offset in samples

max_offset <- as.integer(amount * ref_16th_samples)

  if (max_offset == 0) {
    return(as.integer(position_samples))
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate random offset
  offset <- sample(-max_offset:max_offset, 1)

  # Ensure position doesn't go negative
  result <- as.integer(position_samples + offset)
  max(0L, result)
}

# =============================================================================
# Convenience Functions
# =============================================================================

#' Get samples per beat
#'
#' @description Calculates the number of samples per beat (quarter note) at
#'   the given tempo. Returns floating-point for accuracy in calculations.
#'
#' @param bpm Numeric beats per minute
#' @param sample_rate Integer sample rate in Hz. Default SAMPLE_RATE.
#'
#' @return Numeric samples per beat (not rounded)
#'
#' @examples
#' \dontrun{
#' spb <- get_samples_per_beat(120)  # 22050 samples
#' }
#'
#' @export
get_samples_per_beat <- function(bpm, sample_rate = SAMPLE_RATE) {
  (60 / bpm) * sample_rate
}

#' Get samples per bar
#'
#' @description Calculates the number of samples per bar (4 beats) at the
#'   given tempo. Returns floating-point for accuracy in calculations.
#'
#' @param bpm Numeric beats per minute
#' @param sample_rate Integer sample rate in Hz. Default SAMPLE_RATE.
#'
#' @return Numeric samples per bar (not rounded)
#'
#' @examples
#' \dontrun{
#' spbar <- get_samples_per_bar(120)  # 88200 samples (2 seconds)
#' }
#'
#' @export
get_samples_per_bar <- function(bpm, sample_rate = SAMPLE_RATE) {
  get_samples_per_beat(bpm, sample_rate) * 4
}
