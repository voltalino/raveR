#' @title Glitch Effect Generators
#' @description Audio glitch effects for creative sound design and error handling.
#'   These effects create intentional digital artifacts like stutters, bitcrushing,
#'   and dropouts that can be used musically or to indicate error conditions.
#' @name glitch
NULL

#' Generate Glitch Effect
#'
#' Master function that applies a random combination of glitch effects to audio.
#' The intensity parameter controls the overall severity of the glitching.
#'
#' @param wave A tuneR Wave object to process
#' @param intensity Numeric between 0 and 1 controlling effect severity.
#'   0 = minimal glitching, 1 = maximum glitching. Default 0.5.
#' @param seed Optional random seed for reproducibility. Default NULL.
#'
#' @return A modified Wave object with glitch effects applied
#' @export
#'
#' @details
#' The function randomly selects and combines effects based on intensity:
#' \itemize{
#'   \item Low intensity (0-0.3): Light bitcrushing, occasional stutter
#'   \item Medium intensity (0.3-0.7): Moderate effects, some dropouts
#'   \item High intensity (0.7-1.0): Heavy processing, multiple effects layered
#' }
#'
#' Effects are applied in a musically sensible order to avoid artifacts.
#'
#' @examples
#' \dontrun{
#' # Generate a tone
#' tone <- raver_sine(440, 2)
#'
#' # Light glitching
#' glitched <- generate_glitch_effect(tone, intensity = 0.3)
#'
#' # Heavy glitching
#' glitched_heavy <- generate_glitch_effect(tone, intensity = 0.9)
#'
#' # Reproducible glitching
#' glitch1 <- generate_glitch_effect(tone, intensity = 0.5, seed = 42)
#' glitch2 <- generate_glitch_effect(tone, intensity = 0.5, seed = 42)
#' # glitch1 and glitch2 will be identical
#' }
generate_glitch_effect <- function(wave, intensity = 0.5, seed = NULL) {
  # Validate input
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object", call. = FALSE)
  }

  if (!is.numeric(intensity) || length(intensity) != 1 ||
      intensity < 0 || intensity > 1) {
    stop("intensity must be a single numeric value between 0 and 1", call. = FALSE)
  }

  # Save and restore RNG state
  has_old_seed <- exists(".Random.seed", envir = globalenv())
  if (has_old_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
  }
  on.exit({
    if (has_old_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    # Use wave characteristics for deterministic seeding
    wave_hash <- sum(abs(wave@left[seq(1, min(1000, length(wave@left)), by = 10)]))
    set.seed(as.integer(wave_hash * 1000) %% .Machine$integer.max)
  }

  result <- wave

  # Apply effects based on intensity
  # Scale parameters by intensity

  # Bitcrushing: always apply, intensity affects bit reduction
  bit_reduction <- ceiling(2 + intensity * 6)  # 2-8 bits reduction
  result <- apply_bitcrush(result, bit_reduction = bit_reduction)

  # Stutter: probability increases with intensity
  if (stats::runif(1) < intensity) {
    stutter_count <- ceiling(2 + intensity * 8)  # 2-10 stutters
    stutter_length <- 0.05 + (1 - intensity) * 0.1  # 0.05-0.15 sec
    result <- apply_stutter(result, stutter_count = stutter_count,
                            stutter_length_sec = stutter_length)
  }

  # Dropout: only at medium+ intensity
  if (intensity > 0.3 && stats::runif(1) < intensity) {
    dropout_count <- ceiling(1 + intensity * 5)  # 1-6 dropouts
    dropout_length <- 0.02 + intensity * 0.08  # 0.02-0.10 sec
    result <- apply_dropout(result, dropout_count = dropout_count,
                            dropout_length_sec = dropout_length)
  }

  result
}

#' Apply Stutter Effect
#'
#' Creates a stutter effect by repeating short segments of audio multiple times,
#' creating a rhythmic glitch pattern.
#'
#' @param wave A tuneR Wave object to process
#' @param stutter_count Integer number of stutter events. Default 5.
#' @param stutter_length_sec Numeric duration of each stutter segment in seconds.
#'   Default 0.1.
#' @param repeats Integer number of times to repeat each segment (2-4). Default 3.
#'
#' @return A modified Wave object with stutter effect applied
#' @export
#'
#' @details
#' Stutter positions are chosen pseudo-randomly within the audio. Each stutter
#' replaces the following audio with repeated copies of the stutter segment.
#' This creates the characteristic "CD skip" or "digital glitch" sound.
#'
#' @examples
#' \dontrun{
#' tone <- raver_sine(440, 2)
#' stuttered <- apply_stutter(tone, stutter_count = 3, stutter_length_sec = 0.05)
#' }
apply_stutter <- function(wave, stutter_count = 5L, stutter_length_sec = 0.1,
                          repeats = 3L) {
  # Validate input
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object", call. = FALSE)
  }

  sample_rate <- wave@samp.rate
  n_samples <- length(wave@left)
  stutter_samples <- max(1L, as.integer(stutter_length_sec * sample_rate))

  # Ensure we have enough audio to stutter
  if (n_samples < stutter_samples * 2) {
    return(wave)  # Audio too short, return unchanged
  }

  # Get audio data
  left <- wave@left
  right <- if (wave@stereo) wave@right else NULL

  # Generate stutter positions (avoid edges)
  max_start <- n_samples - stutter_samples * (repeats + 1)
  if (max_start < 1) {
    return(wave)  # Not enough room for stutters
  }

  # Select random positions
  stutter_positions <- sort(sample.int(max_start, min(stutter_count, max_start),
                                        replace = FALSE))

  # Apply stutters (work backwards to avoid index shifting)
  for (pos in rev(stutter_positions)) {
    # Extract segment to repeat
    segment_end <- min(pos + stutter_samples - 1, n_samples)
    segment_left <- left[pos:segment_end]

    # Create repeated segment
    repeated_left <- rep(segment_left, repeats)

    # Calculate replacement region
    replace_end <- min(pos + length(repeated_left) - 1, n_samples)
    replace_length <- replace_end - pos + 1

    # Replace audio
    left[pos:replace_end] <- repeated_left[1:replace_length]

    # Handle stereo
    if (!is.null(right)) {
      segment_right <- right[pos:segment_end]
      repeated_right <- rep(segment_right, repeats)
      right[pos:replace_end] <- repeated_right[1:replace_length]
    }
  }

  # Create output wave
  if (wave@stereo) {
    tuneR::Wave(left = left, right = right, samp.rate = sample_rate,
                bit = wave@bit, pcm = wave@pcm)
  } else {
    tuneR::Wave(left = left, samp.rate = sample_rate,
                bit = wave@bit, pcm = wave@pcm)
  }
}

#' Apply Bitcrush Effect
#'
#' Reduces effective bit depth to create lo-fi digital distortion. This quantizes
#' the audio samples to fewer levels, creating harsh harmonics and aliasing.
#'
#' @param wave A tuneR Wave object to process
#' @param bit_reduction Integer number of bits to reduce. Default 4.
#'   Higher values = more distortion.
#'
#' @return A modified Wave object with bitcrushing applied
#' @export
#'
#' @details
#' Bitcrushing works by quantizing the audio signal to fewer amplitude levels.
#' For example, reducing by 4 bits takes 16-bit audio (65536 levels) down to
#' effectively 12-bit (4096 levels), creating stair-stepped waveforms.
#'
#' @examples
#' \dontrun{
#' tone <- raver_sine(440, 2)
#'
#' # Light crush
#' crushed_light <- apply_bitcrush(tone, bit_reduction = 2)
#'
#' # Heavy crush (very lo-fi)
#' crushed_heavy <- apply_bitcrush(tone, bit_reduction = 8)
#' }
apply_bitcrush <- function(wave, bit_reduction = 4L) {
  # Validate input
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object", call. = FALSE)
  }

  bit_reduction <- as.integer(bit_reduction)
  if (bit_reduction < 1 || bit_reduction > 14) {
    stop("bit_reduction must be between 1 and 14", call. = FALSE)
  }

  # Calculate quantization step
  # For normalized audio (-1 to 1), we quantize to 2^(effective_bits) levels
  effective_bits <- max(2, 16 - bit_reduction)
  levels <- 2^effective_bits
  step <- 2.0 / levels  # Range is 2 (-1 to 1)

  # Normalize to -1,1 range for processing
  left <- wave@left
  right <- if (wave@stereo) wave@right else NULL

  # Determine normalization factor based on bit depth
  if (wave@bit == 16) {
    max_val <- 32767
  } else if (wave@bit == 32) {
    max_val <- 1  # Already normalized
  } else if (wave@bit == 8) {
    max_val <- 127
  } else {
    max_val <- 2^(wave@bit - 1) - 1
  }

  # Normalize to -1,1
  left_norm <- left / max_val
  if (!is.null(right)) {
    right_norm <- right / max_val
  }

  # Quantize
  left_crushed <- round(left_norm / step) * step
  if (!is.null(right)) {
    right_crushed <- round(right_norm / step) * step
  }

  # Scale back
  left_out <- as.integer(round(left_crushed * max_val))
  if (!is.null(right)) {
    right_out <- as.integer(round(right_crushed * max_val))
  }

  # Clamp to valid range
  left_out <- pmax(-max_val, pmin(max_val, left_out))
  if (!is.null(right)) {
    right_out <- pmax(-max_val, pmin(max_val, right_out))
  }

  # Create output wave
  if (wave@stereo) {
    tuneR::Wave(left = left_out, right = right_out, samp.rate = wave@samp.rate,
                bit = wave@bit, pcm = wave@pcm)
  } else {
    tuneR::Wave(left = left_out, samp.rate = wave@samp.rate,
                bit = wave@bit, pcm = wave@pcm)
  }
}

#' Apply Dropout Effect
#'
#' Creates brief silences (dropouts) at random positions in the audio,
#' simulating digital glitches or transmission errors.
#'
#' @param wave A tuneR Wave object to process
#' @param dropout_count Integer number of dropout events. Default 3.
#' @param dropout_length_sec Numeric duration of each dropout in seconds.
#'   Default 0.05.
#'
#' @return A modified Wave object with dropouts applied
#' @export
#'
#' @details
#' Dropout positions are chosen pseudo-randomly. Each dropout replaces audio
#' with silence, creating the effect of momentary signal loss.
#'
#' @examples
#' \dontrun{
#' tone <- raver_sine(440, 2)
#' with_dropouts <- apply_dropout(tone, dropout_count = 5, dropout_length_sec = 0.02)
#' }
apply_dropout <- function(wave, dropout_count = 3L, dropout_length_sec = 0.05) {
  # Validate input
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object", call. = FALSE)
  }

  sample_rate <- wave@samp.rate
  n_samples <- length(wave@left)
  dropout_samples <- max(1L, as.integer(dropout_length_sec * sample_rate))

  # Ensure we have enough audio
  if (n_samples < dropout_samples * 2) {
    return(wave)  # Audio too short
  }

  # Get audio data
  left <- wave@left
  right <- if (wave@stereo) wave@right else NULL

  # Generate dropout positions (avoid very start/end)
  margin <- min(dropout_samples, n_samples %/% 10)
  max_start <- n_samples - dropout_samples - margin
  min_start <- margin

  if (max_start <= min_start) {
    return(wave)  # Not enough room
  }

  # Select random positions
  n_dropouts <- min(dropout_count, (max_start - min_start) %/% dropout_samples)
  if (n_dropouts < 1) {
    return(wave)
  }

  dropout_positions <- sort(sample(min_start:max_start, n_dropouts, replace = FALSE))

  # Apply dropouts
  for (pos in dropout_positions) {
    end_pos <- min(pos + dropout_samples - 1, n_samples)

    # Zero out samples
    left[pos:end_pos] <- 0L

    if (!is.null(right)) {
      right[pos:end_pos] <- 0L
    }
  }

  # Create output wave
  if (wave@stereo) {
    tuneR::Wave(left = left, right = right, samp.rate = wave@samp.rate,
                bit = wave@bit, pcm = wave@pcm)
  } else {
    tuneR::Wave(left = left, samp.rate = wave@samp.rate,
                bit = wave@bit, pcm = wave@pcm)
  }
}
