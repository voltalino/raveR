#' @title Multi-Track Mixing System
#' @description Mixing utilities for combining multiple audio tracks with levels,
#'   normalization, and mastering effects. Provides the final stage of audio
#'   processing before export.
#' @name mixer
NULL

# =============================================================================
# Default Mix Levels
# =============================================================================

#' Default mix levels for each instrument type
#'
#' @description Named list of level multipliers (0-1) for each instrument.
#'   These values are tuned for a balanced deep house mix.
#'
#' @export
DEFAULT_MIX_LEVELS <- list(
  kick = 0.9,
  snare = 0.7,
  clap = 0.6,
  hihat_closed = 0.4,
  hihat_open = 0.5,
  bass = 0.8,
  pad = 0.95,
  lead = 0.6
)

# =============================================================================
# Internal Helpers
# =============================================================================

#' Pad or truncate wave to target length
#'
#' @description Internal helper that adjusts wave length. If wave is shorter
#'   than target, appends silence. If longer, truncates.
#'
#' @param wave A Wave object
#' @param target_samples Target length in samples
#'
#' @return Wave object with exact target length
#' @keywords internal
pad_to_length <- function(wave, target_samples) {
  current_length <- length(wave@left)

  if (current_length == target_samples) {
    return(wave)
  }

  if (current_length > target_samples) {
    # Truncate
    new_samples <- wave@left[seq_len(target_samples)]
  } else {
    # Pad with silence
    new_samples <- c(wave@left, numeric(target_samples - current_length))
  }

  tuneR::Wave(
    left = new_samples,
    samp.rate = wave@samp.rate,
    bit = wave@bit,
    pcm = FALSE
  )
}

# =============================================================================
# Mixing Functions
# =============================================================================

#' Mix multiple tracks together
#'
#' @description Combines multiple Wave objects with level scaling. All tracks
#'   are padded to the same length before mixing.
#'
#' @param tracks Named list of Wave objects. Names should match level names.
#' @param levels Named list of level multipliers (0-1). Names should match
#'   track names. Default uses DEFAULT_MIX_LEVELS.
#'
#' @return Combined Wave object. Note: output may exceed the -1 to 1 range if
#'   tracks are loud. Use normalize_mix() after mixing.
#'
#' @details
#' The mixing process:
#' 1. Find the maximum length among all tracks
#' 2. Pad shorter tracks with silence to match
#' 3. Multiply each track by its level
#' 4. Sum all tracks together
#'
#' @examples
#' \dontrun{
#' tracks <- list(
#'   kick = kick_wave,
#'   bass = bass_wave,
#'   pad = pad_wave
#' )
#' mixed <- mix_tracks(tracks)
#' normalized <- normalize_mix(mixed)
#' }
#'
#' @export
mix_tracks <- function(tracks, levels = DEFAULT_MIX_LEVELS) {
  if (length(tracks) == 0) {
    stop("At least one track is required", call. = FALSE)
  }

  # Validate all tracks are Wave objects
  for (name in names(tracks)) {
    if (!inherits(tracks[[name]], "Wave")) {
      stop("Track '", name, "' is not a Wave object", call. = FALSE)
    }
  }

  # Find maximum length and reference sample rate
  max_length <- 0L
  ref_samp_rate <- NULL

  for (track in tracks) {
    track_length <- length(track@left)
    if (track_length > max_length) {
      max_length <- track_length
    }
    if (is.null(ref_samp_rate)) {
      ref_samp_rate <- track@samp.rate
    }
  }

  # Initialize output buffer
  output <- numeric(max_length)

  # Mix each track
  for (name in names(tracks)) {
    track <- tracks[[name]]

    # Pad to common length
    padded <- pad_to_length(track, max_length)

    # Get level for this track (default to 1.0 if not specified)
    level <- if (name %in% names(levels)) levels[[name]] else 1.0
    level <- max(0.0, min(1.0, level))

    # Add scaled track to output
    output <- output + padded@left * level
  }

  tuneR::Wave(
    left = output,
    samp.rate = ref_samp_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Normalize mix to target headroom
#'
#' @description Normalizes a Wave object so that the peak amplitude matches
#'   a target headroom level. Prevents clipping while maximizing loudness.
#'
#' @param wave A Wave object to normalize
#' @param headroom_db Numeric target headroom in dB below full scale.
#'   Default -3.0 dB (leaves 3 dB headroom).
#'
#' @return Normalized Wave object
#'
#' @details
#' The normalization process:
#' 1. Find the peak amplitude in the wave
#' 2. Calculate the target peak level: 10^(headroom_db/20)
#' 3. Scale the wave by target_peak / current_peak
#'
#' For example, with headroom_db = -3.0:
#' - Target peak = 10^(-3/20) = 0.708
#' - If current peak is 1.5, scale by 0.708/1.5 = 0.472
#'
#' @examples
#' \dontrun{
#' mixed <- mix_tracks(tracks)
#' normalized <- normalize_mix(mixed, headroom_db = -3)
#' }
#'
#' @export
normalize_mix <- function(wave, headroom_db = -3.0) {
  if (!inherits(wave, "Wave")) {
    stop("wave must be a Wave object", call. = FALSE)
  }

  samples <- wave@left
  peak <- max(abs(samples))

  # If silence or very quiet, return as-is
  if (peak < 1e-10) {
    return(wave)
  }

  # Calculate target peak level
  target_peak <- 10^(headroom_db / 20)

  # Calculate and apply normalization factor
  norm_factor <- target_peak / peak
  normalized_samples <- samples * norm_factor

  tuneR::Wave(
    left = normalized_samples,
    samp.rate = wave@samp.rate,
    bit = wave@bit,
    pcm = FALSE
  )
}

#' Apply master limiter
#'
#' @description Applies soft limiting to prevent hard clipping. Uses tanh
#'   saturation for smooth limiting characteristics.
#'
#' @param wave A Wave object to limit
#' @param threshold Numeric threshold level (0-1). Samples exceeding this
#'   are soft-clipped. Default 0.95.
#'
#' @return Limited Wave object
#'
#' @details
#' The limiter uses tanh saturation:
#'   output = tanh(input / threshold) * threshold
#'
#' This provides smooth compression of peaks rather than hard clipping,
#' which sounds more natural and avoids digital distortion artifacts.
#'
#' @examples
#' \dontrun{
#' normalized <- normalize_mix(mixed)
#' limited <- apply_master_limiter(normalized, threshold = 0.95)
#' }
#'
#' @export
apply_master_limiter <- function(wave, threshold = 0.95) {
  if (!inherits(wave, "Wave")) {
    stop("wave must be a Wave object", call. = FALSE)
  }

  # Clamp threshold to valid range
  threshold <- max(0.1, min(1.0, threshold))

  samples <- wave@left

  # Apply tanh saturation: output = tanh(input / threshold) * threshold
  limited_samples <- tanh(samples / threshold) * threshold

  tuneR::Wave(
    left = limited_samples,
    samp.rate = wave@samp.rate,
    bit = wave@bit,
    pcm = FALSE
  )
}

#' Apply master EQ
#'
#' @description Applies optional master EQ for a "finished" sound. Adds
#'   subtle bass boost and high frequency "air".
#'
#' @param wave A Wave object to EQ
#' @param bass_boost_db Numeric bass boost in dB around 60-100 Hz.
#'   Default 2.0 dB.
#' @param air_boost_hz Numeric frequency above which to apply high shelf.
#'   Default 10000 Hz.
#'
#' @return EQ'd Wave object
#'
#' @details
#' This applies simple one-pole filters for subtle tonal shaping:
#' - A low-frequency boost for bass presence
#' - A high-frequency boost for "air" and clarity
#'
#' The effect is subtle and intended for final mastering rather than
#' dramatic tonal changes.
#'
#' @examples
#' \dontrun{
#' limited <- apply_master_limiter(normalized)
#' eq_wave <- apply_master_eq(limited, bass_boost_db = 2)
#' }
#'
#' @export
apply_master_eq <- function(wave, bass_boost_db = 2.0, air_boost_hz = 10000) {
  if (!inherits(wave, "Wave")) {
    stop("wave must be a Wave object", call. = FALSE)
  }

  samples <- wave@left
  sample_rate <- wave@samp.rate
  n_samples <- length(samples)

  if (n_samples == 0) {
    return(wave)
  }

  # Bass boost using lowpass filter mixed with original
  # Extract low frequencies with lowpass around 100 Hz
  bass_cutoff <- 100
  bass_alpha <- bass_cutoff / (bass_cutoff + sample_rate / (2 * pi))

  bass_filtered <- numeric(n_samples)
  bass_filtered[1] <- samples[1] * bass_alpha

  for (i in 2:n_samples) {
    bass_filtered[i] <- (1 - bass_alpha) * bass_filtered[i - 1] + bass_alpha * samples[i]
  }

  # Calculate bass boost amount (linear from dB)
  bass_boost_linear <- 10^(bass_boost_db / 20) - 1.0

  # Add boosted bass to original
  output <- samples + bass_filtered * bass_boost_linear

  # High frequency boost (simple high-pass mixed back in)
  # This is a simple approximation - subtract lowpass from original
  air_alpha <- air_boost_hz / (air_boost_hz + sample_rate / (2 * pi))

  air_lowpass <- numeric(n_samples)
  air_lowpass[1] <- output[1] * air_alpha

  for (i in 2:n_samples) {
    air_lowpass[i] <- (1 - air_alpha) * air_lowpass[i - 1] + air_alpha * output[i]
  }

  # High frequencies = original - lowpass
  air_content <- output - air_lowpass

  # Add subtle high frequency boost (fixed 1.5 dB)
  air_boost_linear <- 10^(1.5 / 20) - 1.0
  output <- output + air_content * air_boost_linear

  tuneR::Wave(
    left = output,
    samp.rate = sample_rate,
    bit = wave@bit,
    pcm = FALSE
  )
}
