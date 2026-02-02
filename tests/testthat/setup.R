# Test setup - helpers for conditional test execution

#' Check if audio playback is available
#'
#' Attempts to play a short silent audio clip to verify audio device access.
#' Returns FALSE on headless servers or when audio device is unavailable.
audio_available <- function() {
  tryCatch({
    # Create a very short silent wave (100 samples)
    silent <- tuneR::Wave(left = rep(0, 100), samp.rate = 44100, bit = 16)
    # Try to play it - this will fail on headless servers
    instance <- audio::play(silent@left / 32768)
    audio::close(instance)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# Cache the result to avoid repeated checks
.audio_available <- NULL

skip_if_no_audio <- function() {

  if (is.null(.audio_available)) {
    .audio_available <<- audio_available()
  }
  if (!.audio_available) {
    testthat::skip("Audio playback not available (headless server)")
  }
}
