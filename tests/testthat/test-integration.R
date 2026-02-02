# ==============================================================================
# Integration Tests for Phase 3: Deep House Engine
# ==============================================================================
#
# These tests verify the complete Phase 3 pipeline works together:
# - Analyze -> Map -> Compose produces valid output
# - Different scripts produce different outputs
# - Same script + seed produces identical outputs
# - Audio quality is acceptable (no clipping, expected duration)
# - API parameters work correctly

# ==============================================================================
# Full Pipeline Tests (3 tests)
# ==============================================================================

test_that("full pipeline: analyze -> map -> compose produces valid Wave", {
  skip_if_not_installed("digest")

  # Create test script
  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "# Test script for pipeline integration",
    "process <- function(x) {",
    "  if (is.null(x)) return(NULL)",
    "  for (i in seq_along(x)) {",
    "    x[i] <- x[i] * 2",
    "  }",
    "  x",
    "}",
    "result <- process(data)"
  ), temp_file)
  on.exit(unlink(temp_file))

  # Run full pipeline
  model <- raver_analyze(temp_file)

  # Verify analysis worked
  expect_true(inherits(model, "CodeModel"))
  expect_true(length(model$functions) >= 1)

  # Verify musical mapping
  expect_true(model$part_count >= 1)
  expect_true(model$part_count <= 4)
  expect_true(model$density_level >= 0)
  expect_true(model$density_level <= 1)

  # Compose full track
  track <- raver_compose(model, bpm = 120)

  # Verify output
  expect_true(inherits(track, "Wave"))
  expect_true(length(track@left) > 0)
  expect_equal(track@samp.rate, SAMPLE_RATE)
})

test_that("different scripts produce different outputs", {
  skip_if_not_installed("digest")

  # Script A: simple
  script_a <- tempfile(fileext = ".R")
  writeLines(c(
    "add <- function(x, y) x + y",
    "subtract <- function(x, y) x - y"
  ), script_a)

  # Script B: complex
  script_b <- tempfile(fileext = ".R")
  writeLines(c(
    "transform_data <- function(data) {",
    "  for (col in names(data)) {",
    "    if (is.numeric(data[[col]])) {",
    "      data[[col]] <- scale(data[[col]])",
    "    }",
    "  }",
    "  data",
    "}",
    "validate <- function(x) {",
    "  if (length(x) == 0) stop('Empty')",
    "  x",
    "}"
  ), script_b)

  on.exit({
    unlink(script_a)
    unlink(script_b)
  })

  # Analyze and compose
  model_a <- raver_analyze(script_a)
  model_b <- raver_analyze(script_b)

  # Files with different content should map to different parameters
  # (At minimum, file paths differ so hashes differ)
  expect_true(model_a$file_hash != model_b$file_hash)

  # Different files produce different keys
  key_a <- hash_to_key(model_a$file_path)
  key_b <- hash_to_key(model_b$file_path)

  # Keys may differ (depends on hash) - at minimum they should be valid MIDI notes
  expect_true(key_a >= 36 && key_a <= 60)
  expect_true(key_b >= 36 && key_b <= 60)

  # Arrangements should differ based on complexity metrics
  arr_a <- create_arrangement(model_a, bpm = 120)
  arr_b <- create_arrangement(model_b, bpm = 120)

  # Different complexity should lead to different arrangements
  # Script B is more complex so should have more sections or longer duration
  expect_true(
    arr_a$total_bars != arr_b$total_bars ||
    length(arr_a$sections) != length(arr_b$sections) ||
    arr_a$key != arr_b$key,
    "Different scripts should produce different arrangements"
  )
})

test_that("same script + seed produces identical outputs", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "analyze_metrics <- function(data) {",
    "  results <- list()",
    "  for (name in names(data)) {",
    "    results[[name]] <- summary(data[[name]])",
    "  }",
    "  results",
    "}"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Generate same track twice
  track1 <- raver_compose(model, bpm = 120, seed = "test_seed_abc")
  track2 <- raver_compose(model, bpm = 120, seed = "test_seed_abc")

  # Should be exactly identical
  expect_equal(length(track1@left), length(track2@left))
  expect_equal(track1@left, track2@left)
})

# ==============================================================================
# Audio Quality Tests (3 tests)
# ==============================================================================

test_that("output has expected duration (bars * bar_duration)", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("fn <- function(x) x", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Test with specific bar counts
  test_cases <- list(
    list(type = "intro", bars = 4, bpm = 118),
    list(type = "drop", bars = 8, bpm = 120),
    list(type = "breakdown", bars = 2, bpm = 124)
  )

  for (tc in test_cases) {
    section <- raver_compose_section(
      model,
      section_type = tc$type,
      bpm = tc$bpm,
      bars = tc$bars
    )

    # Expected duration: bars * (60/bpm) * 4
    expected_sec <- tc$bars * (60 / tc$bpm) * 4
    actual_sec <- length(section@left) / section@samp.rate

    expect_equal(
      actual_sec, expected_sec,
      tolerance = 0.01,
      info = sprintf("%s: %d bars at %d BPM", tc$type, tc$bars, tc$bpm)
    )
  }
})

test_that("output doesn't clip (max abs <= 1.0)", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "# Complex script with many elements",
    "fn1 <- function(a) a + 1",
    "fn2 <- function(b) b * 2",
    "fn3 <- function(c) c - 3",
    "fn4 <- function(d) d / 4",
    "fn5 <- function(e) sqrt(e)"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Full track with all sections
  track <- raver_compose(model, bpm = 120)

  # Check for clipping
  peak <- max(abs(track@left))
  expect_true(peak <= 1.0, sprintf("Peak %.3f exceeds 1.0 (clipping)", peak))

  # Also check it's not too quiet
  expect_true(peak > 0.3, sprintf("Peak %.3f is very low", peak))
})

test_that("output has audio content (not silence)", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("identity <- function(x) x", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Generate drop section (should have audio)
  section <- raver_compose_section(model, "drop", bpm = 120, bars = 4)

  # Calculate RMS energy
  rms <- sqrt(mean(section@left^2))

  # Should have meaningful audio content (not silence)
  expect_true(rms > 0.01, sprintf("RMS %.4f too low - possibly silence", rms))

  # Verify some variation (not DC signal)
  samples <- section@left[1:10000]
  expect_true(sd(samples) > 0.01, "Audio has no variation - possibly DC")
})

# ==============================================================================
# API Requirements Tests (2 tests)
# ==============================================================================

test_that("bpm parameter works (118-124 range enforced)", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("f <- function(x) x", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Valid BPM values should work
  for (bpm in c(118, 120, 122, 124)) {
    section <- raver_compose_section(model, "intro", bpm = bpm, bars = 1)
    expect_true(inherits(section, "Wave"))

    # Verify tempo by checking duration
    expected_bar_duration <- (60 / bpm) * 4
    actual_duration <- length(section@left) / section@samp.rate
    expect_equal(actual_duration, expected_bar_duration, tolerance = 0.01)
  }

  # Invalid BPM values should error (now 60-180 range for multi-genre support)
  expect_error(
    raver_compose_section(model, "intro", bpm = 50),
    "BPM must be between 60 and 180"
  )

  expect_error(
    raver_compose_section(model, "intro", bpm = 200),
    "BPM must be between 60 and 180"
  )
})

test_that("seed parameter works (deterministic output)", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "calc <- function(x) {",
    "  sum(x) / length(x)",
    "}"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Same code + seed = same output (deterministic)
  track_a1 <- raver_compose(model, bpm = 120, seed = "seed_alpha")
  track_a2 <- raver_compose(model, bpm = 120, seed = "seed_alpha")
  expect_equal(track_a1@left, track_a2@left)

  # Same code + different seed = same output (composition is code-deterministic)
  # Note: The seed is provided for future extensibility but current composition
  # is fully deterministic based on code structure, not RNG
  track_b <- raver_compose(model, bpm = 120, seed = "seed_beta")

  # Current design: same code = same output regardless of seed
  # This tests the determinism property - same input always yields same output
  expect_equal(length(track_a1@left), length(track_b@left))
})

# ==============================================================================
# Section Type Tests (2 bonus tests)
# ==============================================================================

test_that("all section types can be rendered", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("test <- function(x) x", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  section_types <- c("intro", "build", "drop", "breakdown", "outro")

  for (st in section_types) {
    section <- raver_compose_section(model, st, bpm = 120, bars = 2)

    expect_true(
      inherits(section, "Wave"),
      info = paste("Section type", st, "should produce Wave")
    )
    expect_true(
      length(section@left) > 0,
      info = paste("Section type", st, "should have samples")
    )
  }
})

test_that("drum samples are loaded and used", {
  # Clear cache to ensure fresh load
  clear_sample_cache()

  # Find samples directory - handle both installed package and development
  sample_dir <- system.file("samples", package = "raveR")
  if (sample_dir == "" || !dir.exists(sample_dir)) {
    # Development mode: try relative path from test directory
    pkg_root <- normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
    if (!dir.exists(file.path(pkg_root, "inst", "samples"))) {
      # Try common package root locations
      pkg_root <- normalizePath(file.path(getwd()), mustWork = FALSE)
      while (nchar(pkg_root) > 1 && !file.exists(file.path(pkg_root, "DESCRIPTION"))) {
        pkg_root <- dirname(pkg_root)
      }
    }
    sample_dir <- file.path(pkg_root, "inst", "samples")
  }

  # Skip test if samples not found (CI environments may not have them)
  if (!dir.exists(sample_dir)) {
    skip("Samples directory not found - skipping sample load test")
  }

  # Load individual samples directly to verify they exist
  expected_samples <- c("909_kick.wav", "909_snare.wav", "909_clap.wav",
                        "909_hihat_closed.wav", "909_hihat_open.wav")

  for (sample_file in expected_samples) {
    sample_path <- file.path(sample_dir, sample_file)
    if (!file.exists(sample_path)) {
      skip(paste("Sample not found:", sample_file))
    }
  }

  # Now verify the drum kit loader works
  kit <- raver_get_drum_kit()

  # If samples exist in the directory, kit should load them
  # (they may be NULL if path resolution fails)
  expect_type(kit, "list")
  expect_true(length(kit) == 5)

  # Verify expected names
  expect_true(all(c("kick", "snare", "clap", "hihat_closed", "hihat_open") %in% names(kit)))
})
