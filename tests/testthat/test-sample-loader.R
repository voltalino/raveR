# Test sample loading infrastructure
# Tests for R/sample_loader.R

# Helper function to create temporary test WAV files
create_test_wav <- function(duration_samples = 4410,
                            samp_rate = 44100,
                            stereo = FALSE,
                            bit = 32) {
  samples <- sin(2 * pi * 440 * seq(0, duration_samples - 1) / samp_rate)

  if (stereo) {
    samples_r <- sin(2 * pi * 880 * seq(0, duration_samples - 1) / samp_rate)
    wave <- tuneR::Wave(
      left = samples,
      right = samples_r,
      samp.rate = samp_rate,
      bit = bit,
      pcm = FALSE
    )
  } else {
    wave <- tuneR::Wave(
      left = samples,
      samp.rate = samp_rate,
      bit = bit,
      pcm = FALSE
    )
  }

  temp_file <- tempfile(fileext = ".wav")
  tuneR::writeWave(wave, temp_file)
  temp_file
}

# =============================================================================
# Basic Loading Tests (3 tests)
# =============================================================================

test_that("raver_load_sample loads valid WAV file and returns Wave object", {
  temp_file <- create_test_wav()
  on.exit(unlink(temp_file))

  loaded <- raver_load_sample(temp_file)

  expect_s4_class(loaded, "Wave")
  expect_true(length(loaded@left) > 0)
})

test_that("loaded sample has correct sample rate (44100)", {
  temp_file <- create_test_wav()
  on.exit(unlink(temp_file))

  loaded <- raver_load_sample(temp_file)

  expect_equal(loaded@samp.rate, 44100)
})

test_that("loaded sample is mono", {
  temp_file <- create_test_wav()
  on.exit(unlink(temp_file))

  loaded <- raver_load_sample(temp_file)

  expect_false(loaded@stereo)
})

# =============================================================================
# Caching Behavior Tests (2 tests)
# =============================================================================

test_that("second load returns cached version (identical)", {
  clear_sample_cache()
  temp_file <- create_test_wav()
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  loaded1 <- raver_load_sample(temp_file)
  loaded2 <- raver_load_sample(temp_file)

  expect_identical(loaded1, loaded2)
})

test_that("force_reload bypasses cache", {
  clear_sample_cache()
  temp_file <- create_test_wav()
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  loaded1 <- raver_load_sample(temp_file)
  loaded2 <- raver_load_sample(temp_file, force_reload = TRUE)

  # Should still be equal content, but not necessarily the same R object
  # (depends on implementation - in our case they'll still be identical
  # because the file didn't change)
  expect_equal(loaded1@left, loaded2@left)
  expect_equal(loaded1@samp.rate, loaded2@samp.rate)
})

# =============================================================================
# Sample Rate Conversion Tests (2 tests)
# =============================================================================

test_that("48000 Hz sample is resampled to 44100 Hz", {
  temp_file <- create_test_wav(samp_rate = 48000, duration_samples = 4800)
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  loaded <- raver_load_sample(temp_file)

  expect_equal(loaded@samp.rate, 44100)
})

test_that("22050 Hz sample is upsampled to 44100 Hz", {
  temp_file <- create_test_wav(samp_rate = 22050, duration_samples = 2205)
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  loaded <- raver_load_sample(temp_file)

  expect_equal(loaded@samp.rate, 44100)
})

# =============================================================================
# Stereo Handling Tests (1 test)
# =============================================================================

test_that("stereo WAV is converted to mono", {
  temp_file <- create_test_wav(stereo = TRUE)
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  loaded <- raver_load_sample(temp_file)

  expect_false(loaded@stereo)
  expect_true(length(loaded@left) > 0)
})

# =============================================================================
# Error Handling Tests (2 tests)
# =============================================================================

test_that("missing file returns NULL with warning", {
  clear_sample_cache()

  expect_warning(
    result <- raver_load_sample("/nonexistent/path/file.wav"),
    "Sample file not found"
  )

  expect_null(result)
})

test_that("empty sample returns NULL with warning", {
  # Create empty WAV file
  empty_wave <- tuneR::Wave(
    left = numeric(0),
    samp.rate = 44100,
    bit = 32,
    pcm = FALSE
  )
  temp_file <- tempfile(fileext = ".wav")
  tuneR::writeWave(empty_wave, temp_file)
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  expect_warning(
    result <- raver_load_sample(temp_file),
    "empty"
  )

  expect_null(result)
})

# =============================================================================
# Cache Management Tests (1 test)
# =============================================================================

test_that("clear_sample_cache empties the cache", {
  temp_file <- create_test_wav()
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  # Load a sample (caches it)
  raver_load_sample(temp_file)

  # Cache should not be empty
  cache_size_before <- length(ls(envir = .sample_cache, all.names = TRUE))
  expect_gt(cache_size_before, 0)

  # Clear cache
  clear_sample_cache()

  # Cache should be empty
  cache_size_after <- length(ls(envir = .sample_cache, all.names = TRUE))
  expect_equal(cache_size_after, 0)
})

# =============================================================================
# Drum Kit Tests (2 tests - test graceful handling of missing samples)
# =============================================================================

test_that("raver_get_drum_kit returns list with expected names", {
  clear_sample_cache()
  on.exit(clear_sample_cache())

  # Suppress warnings about missing sample files
  suppressWarnings({
    kit <- raver_get_drum_kit()
  })

  expect_type(kit, "list")
  expect_named(kit, c("kick", "snare", "clap", "hihat_closed", "hihat_open"))
})

test_that("raver_get_drum_kit handles samples correctly", {
  clear_sample_cache()
  on.exit(clear_sample_cache())

  # Get drum kit (may warn if samples missing, succeeds if present)
  kit <- suppressWarnings(raver_get_drum_kit())

  # Kit structure should always be correct
  expect_type(kit, "list")
  expect_named(kit, c("kick", "snare", "clap", "hihat_closed", "hihat_open"))

  # Each item should be either NULL (missing) or Wave (present)
  for (name in names(kit)) {
    expect_true(is.null(kit[[name]]) || inherits(kit[[name]], "Wave"))
  }
})

# =============================================================================
# Format Normalization Tests (2 additional tests)
# =============================================================================

test_that("loaded sample is 32-bit", {
  temp_file <- create_test_wav()
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  loaded <- raver_load_sample(temp_file)

  expect_equal(loaded@bit, 32)
})

test_that("very short sample loads with warning", {
  # Create very short WAV (< 10 samples)
  short_wave <- tuneR::Wave(
    left = c(0.1, 0.2, 0.3),
    samp.rate = 44100,
    bit = 32,
    pcm = FALSE
  )
  temp_file <- tempfile(fileext = ".wav")
  tuneR::writeWave(short_wave, temp_file)
  on.exit({
    unlink(temp_file)
    clear_sample_cache()
  })

  expect_warning(
    result <- raver_load_sample(temp_file),
    "very short"
  )

  # Should still load successfully
  expect_s4_class(result, "Wave")
})
