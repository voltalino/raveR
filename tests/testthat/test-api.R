# Test composeR user-facing API

test_that("composeR creates valid WAV file", {
  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  output_file <- tempfile(fileext = ".wav")
  on.exit({
    unlink(script_file)
    unlink(output_file)
  }, add = TRUE)

  # Write a simple R script
  writeLines(c(
    "x <- 1",
    "y <- function(a) { a + 1 }",
    "z <- y(x)"
  ), script_file)

  # Should produce a valid WAV file
  result <- composeR(script_file, output_file)

  expect_true(file.exists(output_file))
  expect_equal(result, output_file)

  # Verify it's a valid WAV file readable by tuneR
  wave <- tuneR::readWave(output_file)
  expect_s4_class(wave, "Wave")
  expect_gt(length(wave@left), 0)
})

test_that("composeR creates valid MP3 file", {
  skip_if_not(suppressWarnings(check_ffmpeg()), "FFmpeg not available")

  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  output_file <- tempfile(fileext = ".mp3")
  on.exit({
    unlink(script_file)
    unlink(output_file)
  }, add = TRUE)

  # Write a simple R script
  writeLines(c(
    "x <- 1",
    "y <- function(a) { a + 1 }",
    "z <- y(x)"
  ), script_file)

  # Should produce a valid MP3 file
  result <- composeR(script_file, output_file)

  expect_true(file.exists(output_file))
  expect_equal(result, output_file)
  expect_gt(file.size(output_file), 0)
})

test_that("composeR errors on non-existent script", {
  output_file <- tempfile(fileext = ".wav")

  expect_error(
    composeR("/nonexistent/path/script.R", output_file),
    "Script not found"
  )
})

test_that("composeR errors on invalid output format", {
  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  on.exit(unlink(script_file), add = TRUE)

  writeLines("x <- 1", script_file)

  expect_error(
    composeR(script_file, "output.ogg"),
    "Unsupported output format"
  )

  expect_error(
    composeR(script_file, "output.flac"),
    "Unsupported output format"
  )
})

test_that("composeR errors when output has no extension", {
  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  on.exit(unlink(script_file), add = TRUE)

  writeLines("x <- 1", script_file)

  expect_error(
    composeR(script_file, "output_no_extension"),
    "Cannot determine output format"
  )
})

test_that("composeR respects bpm parameter", {
  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  output_file <- tempfile(fileext = ".wav")
  on.exit({
    unlink(script_file)
    unlink(output_file)
  }, add = TRUE)

  writeLines("x <- 1", script_file)

  # Should not error with valid BPM
  expect_no_error(composeR(script_file, output_file, bpm = 120))
  expect_true(file.exists(output_file))
})

test_that("composeR handles parse errors gracefully", {
  # Create a script with syntax error
  script_file <- tempfile(fileext = ".R")
  output_file <- tempfile(fileext = ".wav")
  on.exit({
    unlink(script_file)
    unlink(output_file)
  }, add = TRUE)

  writeLines(c(
    "x <- 1 +",  # Incomplete expression - parse error
    "function( {",  # Syntax error
    "}"
  ), script_file)

  # Should produce output with warning, not error
  expect_warning(
    composeR(script_file, output_file),
    "Parse error"
  )

  # File should still be created (graceful degradation)
  expect_true(file.exists(output_file))
})

test_that("composeR produces deterministic output with same seed", {
  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  output_file1 <- tempfile(fileext = ".wav")
  output_file2 <- tempfile(fileext = ".wav")
  on.exit({
    unlink(script_file)
    unlink(output_file1)
    unlink(output_file2)
  }, add = TRUE)

  writeLines(c(
    "x <- 1",
    "y <- function(a) { a + 1 }"
  ), script_file)

  # Run twice with same seed
  composeR(script_file, output_file1, seed = 42)
  composeR(script_file, output_file2, seed = 42)

  # Files should be identical
  wave1 <- tuneR::readWave(output_file1)
  wave2 <- tuneR::readWave(output_file2)

  expect_equal(wave1@left, wave2@left)
})

test_that("detect_output_format handles various extensions", {
  expect_equal(detect_output_format("output.wav"), "wav")
  expect_equal(detect_output_format("output.WAV"), "wav")
  expect_equal(detect_output_format("output.Wav"), "wav")
  expect_equal(detect_output_format("output.mp3"), "mp3")
  expect_equal(detect_output_format("output.MP3"), "mp3")
  expect_equal(detect_output_format("/path/to/file.wav"), "wav")
  expect_equal(detect_output_format("file.with.dots.wav"), "wav")
})

test_that("detect_output_format errors on unsupported formats", {
  expect_error(detect_output_format("output.ogg"), "Unsupported output format")
  expect_error(detect_output_format("output.flac"), "Unsupported output format")
  expect_error(detect_output_format("output.aac"), "Unsupported output format")
})

test_that("detect_output_format errors on missing extension", {
  expect_error(detect_output_format("output"), "Cannot determine output format")
  expect_error(detect_output_format("noextension"), "Cannot determine output format")
})

# ============================================================================
# playR() and stopR() API tests
# ============================================================================

test_that("playR errors on non-existent script", {
  expect_error(
    playR("/nonexistent/path/script.R"),
    "Script not found"
  )
})

test_that("stopR doesn't error when no playback active", {
  # Ensure no controller is active
  .live_env$controller <- NULL

  # Should not error, just message
  expect_message(stopR(), "No active playback")
})

test_that("playR creates controller in .live_env", {
  skip_if_no_audio()

  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  on.exit({
    unlink(script_file)
    # Clean up controller
    if (!is.null(.live_env$controller)) {
      .live_env$controller$stop()
      .live_env$controller <- NULL
    }
  }, add = TRUE)

  writeLines(c(
    "x <- 1",
    "y <- function(a) { a + 1 }"
  ), script_file)

  # Start playback - will create controller
  expect_message(playR(script_file), "Playing:")

  # Verify controller exists

  expect_false(is.null(.live_env$controller))
  expect_true(inherits(.live_env$controller, "PlaybackController"))
  expect_true(.live_env$controller$is_playing)

  # Stop playback
  stopR()
})

test_that("stopR clears controller from .live_env", {
  skip_if_no_audio()

  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  on.exit({
    unlink(script_file)
    # Clean up controller just in case
    if (!is.null(.live_env$controller)) {
      .live_env$controller$stop()
      .live_env$controller <- NULL
    }
  }, add = TRUE)

  writeLines("x <- 1", script_file)

  # Start playback
  playR(script_file)
  expect_false(is.null(.live_env$controller))

  # Stop playback
  expect_message(stopR(), "Playback stopped")

  # Verify controller is cleared
  expect_null(.live_env$controller)
})

test_that("playR stops previous playback before starting new", {
  skip_if_no_audio()

  # Create two test scripts
  script_file1 <- tempfile(fileext = ".R")
  script_file2 <- tempfile(fileext = ".R")
  on.exit({
    unlink(script_file1)
    unlink(script_file2)
    # Clean up controller
    if (!is.null(.live_env$controller)) {
      .live_env$controller$stop()
      .live_env$controller <- NULL
    }
  }, add = TRUE)

  writeLines("x <- 1", script_file1)
  writeLines("y <- 2", script_file2)

  # Start first playback
  playR(script_file1)
  controller1 <- .live_env$controller

  # Start second playback - should stop first
  playR(script_file2)
  controller2 <- .live_env$controller

  # Second controller should be different and first should be stopped
  expect_false(identical(controller1, controller2))
  expect_false(controller1$is_playing)

  # Clean up
  stopR()
})

test_that("playR accepts custom bpm parameter", {
  skip_if_no_audio()

  # Create a simple test script
  script_file <- tempfile(fileext = ".R")
  on.exit({
    unlink(script_file)
    # Clean up controller
    if (!is.null(.live_env$controller)) {
      .live_env$controller$stop()
      .live_env$controller <- NULL
    }
  }, add = TRUE)

  writeLines("x <- 1", script_file)

  # Start playback with custom BPM
  playR(script_file, bpm = 122)

  # Verify BPM was set
  expect_equal(.live_env$controller$bpm, 122L)

  # Stop playback
  stopR()
})

test_that(".live_env is a package-level environment", {
  # Verify the live environment exists and is an environment
  expect_true(exists(".live_env"))
  expect_true(is.environment(.live_env))
  expect_true(is.null(.live_env$controller) || inherits(.live_env$controller, "PlaybackController"))
})
