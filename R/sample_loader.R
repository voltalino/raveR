#' @title Sample Loading and Caching
#' @description Load, resample, and cache drum samples for deep house production.
#'   Handles WAV files at any sample rate/bit depth and normalizes them to
#'   the internal format (44100 Hz, 32-bit float, mono).
#' @name sample-loading
NULL

# Package environment for sample cache
# Using emptyenv() as parent prevents lookup in parent environments
.sample_cache <- new.env(parent = emptyenv())

#' Resample a Wave object to target sample rate
#'
#' Uses linear interpolation to convert sample rate.
#'
#' @param wave A Wave object
#' @param target_rate Target sample rate (default: SAMPLE_RATE = 44100)
#' @return Wave object at target sample rate
#' @keywords internal
resample_wave <- function(wave, target_rate = SAMPLE_RATE) {
  if (wave@samp.rate == target_rate) {
    return(wave)
  }

  source_rate <- wave@samp.rate
  source_length <- length(wave@left)

  # Calculate new length based on ratio
  ratio <- target_rate / source_rate
  new_length <- round(source_length * ratio)

  if (new_length == 0) {
    return(wave)
  }

  # Create new sample positions
  old_positions <- seq(1, source_length, length.out = source_length)
  new_positions <- seq(1, source_length, length.out = new_length)

  # Interpolate left channel
  new_left <- stats::approx(old_positions, wave@left, xout = new_positions,
                            method = "linear", rule = 2)$y

  # Handle right channel if stereo
  if (wave@stereo) {
    new_right <- stats::approx(old_positions, wave@right, xout = new_positions,
                               method = "linear", rule = 2)$y
    tuneR::Wave(left = new_left, right = new_right,
                samp.rate = target_rate, bit = wave@bit)
  } else {
    tuneR::Wave(left = new_left, samp.rate = target_rate, bit = wave@bit)
  }
}

#' Convert stereo Wave to mono
#'
#' Averages left and right channels.
#'
#' @param wave A Wave object
#' @return Mono Wave object
#' @keywords internal
stereo_to_mono <- function(wave) {
  if (!wave@stereo) {
    return(wave)
  }

  # Average left and right channels
  mono_samples <- (wave@left + wave@right) / 2

  tuneR::Wave(left = mono_samples, samp.rate = wave@samp.rate, bit = wave@bit)
}

#' Normalize sample to internal format
#'
#' Converts sample to standard internal format:
#' - 44100 Hz sample rate
#' - 32-bit float
#' - Mono
#'
#' @param wave A Wave object
#' @return Normalized Wave object
#' @keywords internal
normalize_sample_format <- function(wave) {
  # Convert to mono first (before resampling for efficiency)
  wave <- stereo_to_mono(wave)

  # Resample to target rate
  wave <- resample_wave(wave, SAMPLE_RATE)

  # Convert to 32-bit float format
  # If already 32-bit, just ensure it's the right format
  if (wave@bit != BIT_DEPTH) {
    # Get samples as numeric (normalized to [-1, 1] range if needed)
    samples <- wave@left

    # Normalize based on original bit depth
    if (wave@bit == 16) {
      samples <- samples / 32768
    } else if (wave@bit == 24) {
      samples <- samples / 8388608
    } else if (wave@bit == 8) {
      samples <- (samples - 128) / 128
    }
    # 32-bit should already be normalized

    wave <- tuneR::Wave(left = samples, samp.rate = SAMPLE_RATE,
                        bit = BIT_DEPTH, pcm = FALSE)
  }

  wave
}

#' Load a WAV sample
#'
#' Loads a WAV file, normalizes its format, and caches it for efficient
#' subsequent access.
#'
#' @param sample_path Path to the WAV file
#' @param force_reload Logical; if TRUE, bypass cache and reload from disk
#' @return Wave object, or NULL if file is missing/corrupt
#' @export
#' @examples
#' \dontrun{
#' kick <- raver_load_sample("inst/samples/909_kick.wav")
#' }
raver_load_sample <- function(sample_path, force_reload = FALSE) {
  # Check cache first (unless force_reload)
  cache_key <- sample_path
  if (!force_reload && exists(cache_key, envir = .sample_cache)) {
    return(get(cache_key, envir = .sample_cache))
  }

  # Check if file exists
  if (!file.exists(sample_path)) {
    warning("Sample file not found: ", sample_path, call. = FALSE)
    return(NULL)
  }

  # Try to load the WAV file
  wave <- tryCatch({
    tuneR::readWave(sample_path)
  }, error = function(e) {
    warning("Error reading sample file: ", sample_path, " - ", e$message,
            call. = FALSE)
    NULL
  })

  if (is.null(wave)) {
    return(NULL)
  }

  # Check for empty samples
  if (length(wave@left) == 0) {
    warning("Sample file is empty: ", sample_path, call. = FALSE)
    return(NULL)
  }

  # Warn about very short samples
  if (length(wave@left) < 10) {
    warning("Sample file is very short (< 10 samples): ", sample_path,
            call. = FALSE)
  }

  # Normalize format
  wave <- normalize_sample_format(wave)

  # Cache the normalized sample
  assign(cache_key, wave, envir = .sample_cache)

  wave
}

#' Get complete 909 drum kit
#'
#' Loads all standard 909 drum samples from the package's inst/samples
#' directory.
#'
#' @return Named list with Wave objects for each drum:
#'   \itemize{
#'     \item kick: Bass drum
#'     \item snare: Snare drum
#'     \item clap: Hand clap
#'     \item hihat_closed: Closed hi-hat
#'     \item hihat_open: Open hi-hat
#'   }
#'   Elements will be NULL if samples are missing.
#' @export
#' @examples
#' \dontrun{
#' kit <- raver_get_drum_kit()
#' if (!is.null(kit$kick)) {
#'   # Use kick sample
#' }
#' }
raver_get_drum_kit <- function() {
  # Check cache for entire kit
  cache_key <- ".drum_kit"
  if (exists(cache_key, envir = .sample_cache)) {
    return(get(cache_key, envir = .sample_cache))
  }

  # Define sample files
  sample_names <- c(
    kick = "909_kick.wav",
    snare = "909_snare.wav",
    clap = "909_clap.wav",
    hihat_closed = "909_hihat_closed.wav",
    hihat_open = "909_hihat_open.wav"
  )

  # Load each sample using system.file for package-relative paths
  kit <- lapply(sample_names, function(filename) {
    sample_path <- system.file("samples", filename, package = "raveR")

    # If running in development (not installed), try relative path
    if (sample_path == "") {
      sample_path <- file.path("inst", "samples", filename)
    }

    raver_load_sample(sample_path)
  })

  names(kit) <- names(sample_names)

  # Cache the kit
  assign(cache_key, kit, envir = .sample_cache)

  kit
}

#' Clear sample cache
#'
#' Removes all cached samples from memory. Useful for testing or
#' when memory needs to be freed.
#'
#' @return Invisible NULL
#' @export
#' @examples
#' clear_sample_cache()
clear_sample_cache <- function() {
  rm(list = ls(envir = .sample_cache, all.names = TRUE), envir = .sample_cache)
  invisible(NULL)
}
