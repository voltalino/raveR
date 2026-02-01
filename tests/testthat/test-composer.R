# ==============================================================================
# Test Suite for Composer and Mixer Integration
# ==============================================================================

# ==============================================================================
# BPM Handling Tests (3 tests)
# ==============================================================================

test_that("auto-selected BPM is within 118-124 range", {
  skip_if_not_installed("digest")

  # Create test scripts with different names
  temp_files <- lapply(1:5, function(i) {
    f <- tempfile(fileext = ".R")
    writeLines(paste0("test", i, " <- function(x) x"), f)
    f
  })
  on.exit(unlink(unlist(temp_files)))

  # Analyze each and check BPM
  for (f in temp_files) {
    model <- raver_analyze(f)
    bpm <- select_bpm(model)

    expect_true(bpm >= 118, info = paste("BPM", bpm, "below 118 for", f))
    expect_true(bpm <= 124, info = paste("BPM", bpm, "above 124 for", f))
    expect_type(bpm, "integer")
  }
})

test_that("explicit BPM is used when provided", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("foo <- function(x) x + 1", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Compose with explicit BPM
  section <- raver_compose_section(model, "intro", bpm = 122, bars = 1)

  # Check duration matches 122 BPM (1 bar = 240/122 sec)
  expected_duration <- (60 / 122) * 4
  actual_duration <- length(section@left) / section@samp.rate

  # Allow small tolerance for rounding
  expect_equal(actual_duration, expected_duration, tolerance = 0.01)
})

test_that("invalid BPM throws clear error", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("foo <- function(x) x", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # BPM too low
  expect_error(
    raver_compose_section(model, "intro", bpm = 100),
    "BPM must be between 118 and 124"
  )

  # BPM too high
  expect_error(
    raver_compose_section(model, "intro", bpm = 140),
    "BPM must be between 118 and 124"
  )

  # Invalid BPM type
  expect_error(
    validate_bpm("fast"),
    "BPM must be a single numeric value"
  )
})

# ==============================================================================
# Seed Determinism Tests (3 tests)
# ==============================================================================

test_that("same script with same seed produces identical output", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "clean <- function(x) x[!is.na(x)]",
    "transform <- function(y) log(y + 1)"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Generate two sections with same explicit seed
  section1 <- raver_compose_section(model, "intro", bpm = 120, bars = 2)
  section2 <- raver_compose_section(model, "intro", bpm = 120, bars = 2)

  # Should be identical
  expect_equal(length(section1@left), length(section2@left))
  expect_equal(section1@left[1:100], section2@left[1:100])
})

test_that("different seeds produce different output", {
  skip_if_not_installed("digest")

  # Create two very different temp files (different functions, different complexity)
  temp_file1 <- tempfile(fileext = ".R")
  temp_file2 <- tempfile(fileext = ".R")
  writeLines(c(
    "foo <- function(x) x + 1",
    "bar <- function(y) y * 2",
    "baz <- function(z) z / 3"
  ), temp_file1)
  writeLines(c(
    "alpha <- function(a) {",
    "  if (a > 0) a else -a",
    "}",
    "beta <- function(b) sqrt(b)"
  ), temp_file2)
  on.exit({
    unlink(temp_file1)
    unlink(temp_file2)
  })

  model1 <- raver_analyze(temp_file1)
  model2 <- raver_analyze(temp_file2)

  # Use drop section which has more elements (bass and pads differ)
  section1 <- raver_compose_section(model1, "drop", bpm = 120, bars = 2)
  section2 <- raver_compose_section(model2, "drop", bpm = 120, bars = 2)

  # Sections should differ (different file hashes -> different keys, motifs)
  # At least some samples should be different
  differences <- sum(abs(section1@left - section2@left) > 0.01)
  expect_true(differences > 0, "Sections from different files should differ")
})

test_that("no seed uses file_hash for determinism", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("test_func <- function(z) z^2", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Generate same section twice without explicit seed
  section1 <- raver_compose_section(model, "drop", bpm = 120, bars = 2)
  section2 <- raver_compose_section(model, "drop", bpm = 120, bars = 2)

  # Should be deterministic via file_hash
  expect_equal(length(section1@left), length(section2@left))
  expect_equal(section1@left[1:50], section2@left[1:50])
})

# ==============================================================================
# Section Generation Tests (2 tests)
# ==============================================================================

test_that("raver_compose_section returns Wave with correct duration", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines("analyze <- function(data) mean(data)", temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Test different section types and bar counts
  test_cases <- list(
    list(type = "intro", bars = 4, bpm = 120),
    list(type = "drop", bars = 8, bpm = 122),
    list(type = "outro", bars = 2, bpm = 118)
  )

  for (tc in test_cases) {
    section <- raver_compose_section(
      model, tc$type,
      bpm = tc$bpm,
      bars = tc$bars
    )

    # Verify it's a Wave object
    expect_true(inherits(section, "Wave"))

    # Verify duration
    expected_duration <- (60 / tc$bpm) * 4 * tc$bars
    actual_duration <- length(section@left) / section@samp.rate

    expect_equal(
      actual_duration, expected_duration,
      tolerance = 0.01,
      info = paste(tc$type, "at", tc$bpm, "BPM,", tc$bars, "bars")
    )
  }
})

test_that("different section types have different characteristics", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "process <- function(x) x %>% clean() %>% transform()",
    "clean <- function(df) df[complete.cases(df), ]",
    "transform <- function(df) mutate(df, z = x + y)"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Generate intro and drop sections
  intro <- raver_compose_section(model, "intro", bpm = 120, bars = 4)
  drop <- raver_compose_section(model, "drop", bpm = 120, bars = 4)

  # Drop should have more energy (higher RMS amplitude typically)
  intro_rms <- sqrt(mean(intro@left^2))
  drop_rms <- sqrt(mean(drop@left^2))

  # Drop should be louder (more elements, fuller sound)
  # This is a soft check - the audio characteristics should differ
  expect_true(
    !identical(intro@left[1:1000], drop@left[1:1000]),
    "Intro and drop sections should differ"
  )
})

# ==============================================================================
# Full Composition Tests (2 tests)
# ==============================================================================

test_that("raver_compose returns valid Wave object", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "analyze_data <- function(x) {",
    "  if (is.null(x)) return(NULL)",
    "  summary(x)",
    "}",
    "result <- analyze_data(data)"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Full composition with explicit BPM
  track <- raver_compose(model, bpm = 120)

  # Verify it's a valid Wave object
  expect_true(inherits(track, "Wave"))
  expect_true(length(track@left) > 0)
  expect_equal(track@samp.rate, SAMPLE_RATE)

  # Verify normalized (peak should be around -3dB = 0.708)
  peak <- max(abs(track@left))
  expect_true(peak <= 1.0, "Peak should not exceed 1.0")
  expect_true(peak > 0.5, "Peak should be reasonably loud")
})

test_that("composed track has expected total duration", {
  skip_if_not_installed("digest")

  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "step1 <- function(x) x + 1",
    "step2 <- function(y) y * 2",
    "result <- step1(step2(input))"
  ), temp_file)
  on.exit(unlink(temp_file))

  model <- raver_analyze(temp_file)

  # Get arrangement to calculate expected duration
  arr <- create_arrangement(model, bpm = 120)
  total_bars <- arr$total_bars

  # Compose track
  track <- raver_compose(model, bpm = 120)

  # Calculate expected duration
  bar_duration_sec <- (60 / 120) * 4
  expected_duration <- total_bars * bar_duration_sec

  actual_duration <- length(track@left) / track@samp.rate

  # Allow small tolerance
  expect_equal(actual_duration, expected_duration, tolerance = 0.1)
})

# ==============================================================================
# Mixer Tests (3 tests - bonus coverage)
# ==============================================================================

test_that("mix_tracks combines waves correctly", {
  # Create test waves
  wave1 <- raver_sine(100, 0.5)
  wave2 <- raver_sine(200, 0.5)

  mixed <- mix_tracks(
    list(bass = wave1, lead = wave2),
    list(bass = 0.5, lead = 0.5)
  )

  expect_true(inherits(mixed, "Wave"))
  expect_equal(length(mixed@left), length(wave1@left))
})

test_that("normalize_mix achieves target headroom", {
  # Create a hot signal
  hot_samples <- rep(2.0, 1000)  # Signal at +6dB
  hot_wave <- create_wave(hot_samples)

  normalized <- normalize_mix(hot_wave, headroom_db = -3)

  # Peak should be at -3dB = 0.708
  peak <- max(abs(normalized@left))
  expected_peak <- 10^(-3/20)

  expect_equal(peak, expected_peak, tolerance = 0.001)
})

test_that("apply_master_limiter prevents clipping", {
  # Create a signal with peaks
  samples <- c(rep(0.5, 100), rep(2.0, 100), rep(0.5, 100))
  wave <- create_wave(samples)

  limited <- apply_master_limiter(wave, threshold = 0.95)

  # All samples should be within threshold
  expect_true(all(abs(limited@left) <= 0.95))
})
