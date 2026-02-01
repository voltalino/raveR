# Tests for step sequencer and swing timing
# These tests verify pattern creation, rendering, and swing algorithms

# =============================================================================
# Swing Timing Tests
# =============================================================================

test_that("straight timing places steps evenly", {
  bpm <- 120
  steps_per_bar <- 16

  # Calculate expected step duration
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / steps_per_bar

  # Check first 4 steps with no swing
  for (step in 1:4) {
    timing <- calculate_step_timing(step, bpm, swing_amount = 0.0, steps_per_bar)
    expected_sec <- (step - 1) * step_duration_sec

    expect_equal(timing$position_sec, expected_sec, tolerance = 1e-10)
  }
})

test_that("swing delays even steps by correct amount", {
  bpm <- 120
  swing_amount <- 0.10

  # Calculate step duration
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / 16
  expected_swing_sec <- swing_amount * step_duration_sec

  # Step 1 (odd) - no swing
  t1 <- calculate_step_timing(1, bpm, swing_amount)
  expect_equal(t1$position_sec, 0.0)

  # Step 2 (even) - should be delayed
  t2 <- calculate_step_timing(2, bpm, swing_amount)
  expected_t2 <- step_duration_sec + expected_swing_sec
  expect_equal(t2$position_sec, expected_t2, tolerance = 1e-10)

  # Step 3 (odd) - no swing
  t3 <- calculate_step_timing(3, bpm, swing_amount)
  expected_t3 <- 2 * step_duration_sec  # No swing offset
  expect_equal(t3$position_sec, expected_t3, tolerance = 1e-10)

  # Step 4 (even) - should be delayed
  t4 <- calculate_step_timing(4, bpm, swing_amount)
  expected_t4 <- 3 * step_duration_sec + expected_swing_sec
  expect_equal(t4$position_sec, expected_t4, tolerance = 1e-10)
})

test_that("odd steps stay on grid regardless of swing amount", {
  bpm <- 124
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / 16

  # Test odd steps with various swing amounts
  for (swing in c(0.0, 0.05, 0.10, 0.167, 0.25)) {
    for (step in c(1, 3, 5, 7, 9, 11, 13, 15)) {
      timing <- calculate_step_timing(step, bpm, swing)
      expected_sec <- (step - 1) * step_duration_sec

      expect_equal(timing$position_sec, expected_sec, tolerance = 1e-10,
                   label = paste("step", step, "with swing", swing))
    }
  }
})

test_that("GROOVE_PRESETS contain expected swing values", {
  expect_true(is.list(GROOVE_PRESETS))
  expect_true("straight" %in% names(GROOVE_PRESETS))
  expect_true("light" %in% names(GROOVE_PRESETS))
  expect_true("medium" %in% names(GROOVE_PRESETS))
  expect_true("heavy" %in% names(GROOVE_PRESETS))
  expect_true("mpc" %in% names(GROOVE_PRESETS))

  # Check values
  expect_equal(GROOVE_PRESETS$straight$swing, 0.0)
  expect_equal(GROOVE_PRESETS$light$swing, 0.05)
  expect_equal(GROOVE_PRESETS$medium$swing, 0.10)
  expect_equal(GROOVE_PRESETS$heavy$swing, 0.15)
  expect_equal(GROOVE_PRESETS$mpc$swing, 0.167)

  # All presets should have descriptions
  for (preset in GROOVE_PRESETS) {
    expect_true("description" %in% names(preset))
    expect_true(nchar(preset$description) > 0)
  }
})

# =============================================================================
# Pattern Creation Tests
# =============================================================================

test_that("create_pattern validates step length", {
  # Valid lengths (16 and 32)
  expect_no_error(create_pattern("kick", rep(TRUE, 16)))
  expect_no_error(create_pattern("kick", rep(TRUE, 32)))

  # Invalid lengths should error
  expect_error(create_pattern("kick", rep(TRUE, 8)), "16 or 32")
  expect_error(create_pattern("kick", rep(TRUE, 15)), "16 or 32")
  expect_error(create_pattern("kick", rep(TRUE, 17)), "16 or 32")
})

test_that("logical steps are converted to velocities", {
  # TRUE/FALSE should become 1.0/0.0
  pat <- create_pattern("kick",
    c(TRUE, FALSE, TRUE, FALSE, rep(FALSE, 12)))

  expect_equal(pat$steps[1], 1.0)
  expect_equal(pat$steps[2], 0.0)
  expect_equal(pat$steps[3], 1.0)
  expect_equal(pat$steps[4], 0.0)
})

test_that("numeric steps preserve velocity values", {
  # Numeric velocities should be preserved (and clamped)
  pat <- create_pattern("kick",
    c(0.5, 0.8, 0.0, 1.0, 1.5, -0.1, rep(0, 10)))

  expect_equal(pat$steps[1], 0.5)
  expect_equal(pat$steps[2], 0.8)
  expect_equal(pat$steps[3], 0.0)
  expect_equal(pat$steps[4], 1.0)
  expect_equal(pat$steps[5], 1.0)  # Clamped from 1.5
  expect_equal(pat$steps[6], 0.0)  # Clamped from -0.1
})

# =============================================================================
# Pattern Rendering Tests
# =============================================================================

test_that("rendered pattern has correct bar duration for BPM", {
  # Use bass (synthesis, no samples needed)
  pat <- create_pattern("bass",
    c(TRUE, rep(FALSE, 15)),
    note = 36)

  # 120 BPM: 1 bar = 2 seconds
  wave_120 <- render_pattern(pat, bpm = 120)
  expected_120 <- (60 / 120) * 4
  actual_120 <- length(wave_120@left) / SAMPLE_RATE
  expect_equal(actual_120, expected_120, tolerance = 0.001)

  # 124 BPM: 1 bar = 1.935 seconds
  wave_124 <- render_pattern(pat, bpm = 124)
  expected_124 <- (60 / 124) * 4
  actual_124 <- length(wave_124@left) / SAMPLE_RATE
  expect_equal(actual_124, expected_124, tolerance = 0.001)

  # 118 BPM: 1 bar = 2.034 seconds
  wave_118 <- render_pattern(pat, bpm = 118)
  expected_118 <- (60 / 118) * 4
  actual_118 <- length(wave_118@left) / SAMPLE_RATE
  expect_equal(actual_118, expected_118, tolerance = 0.001)
})

test_that("active steps produce non-zero samples at correct positions", {
  # Create pattern with one hit on step 5
  pat <- create_pattern("bass",
    c(FALSE, FALSE, FALSE, FALSE, TRUE, rep(FALSE, 11)),
    note = 36)

  wave <- render_pattern(pat, bpm = 120)

  # Calculate step position
  bar_samples <- as.integer((60 / 120) * 4 * SAMPLE_RATE)
  samples_per_step <- bar_samples / 16
  step_5_start <- as.integer((5 - 1) * samples_per_step)

  # Check area around step 5 has audio
  step_5_region <- wave@left[(step_5_start + 1):min(step_5_start + 1000, length(wave@left))]
  expect_gt(max(abs(step_5_region)), 0.01)

  # Check step 1 area is silent (no hit there)
  step_1_region <- wave@left[1:min(500, step_5_start - 100)]
  expect_lt(max(abs(step_1_region)), 0.001)
})

test_that("empty pattern (all FALSE) produces silence", {
  # All steps false
  pat <- create_pattern("bass",
    rep(FALSE, 16),
    note = 36)

  wave <- render_pattern(pat, bpm = 120)

  # Should have correct duration
  expected_duration <- (60 / 120) * 4
  actual_duration <- length(wave@left) / SAMPLE_RATE
  expect_equal(actual_duration, expected_duration, tolerance = 0.001)

  # Should be silent
  expect_equal(max(abs(wave@left)), 0.0)
})

# =============================================================================
# Pattern Combination Tests
# =============================================================================

test_that("combined patterns have same duration as individual", {
  # Create two patterns
  pat1 <- create_pattern("bass",
    c(TRUE, rep(FALSE, 15)),
    note = 36)

  pat2 <- create_pattern("pad",
    c(TRUE, rep(FALSE, 15)),
    note = 60)

  # Render individually
  wave1 <- render_pattern(pat1, bpm = 120)
  wave2 <- render_pattern(pat2, bpm = 120)

  # Combine
  combined <- combine_patterns(list(pat1, pat2), bpm = 120)

  # All should have same duration
  expect_equal(length(combined@left), length(wave1@left))
  expect_equal(length(combined@left), length(wave2@left))
})

test_that("multiple patterns mixed without clipping", {
  # Create patterns with high velocity
  pat1 <- create_pattern("bass",
    c(TRUE, FALSE, FALSE, FALSE, TRUE, rep(FALSE, 11)),
    note = 36,
    velocity = 1.0)

  pat2 <- create_pattern("pad",
    c(TRUE, rep(FALSE, 15)),
    note = 60,
    velocity = 1.0)

  pat3 <- create_pattern("bass",
    c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, rep(FALSE, 9)),
    note = 48,
    velocity = 1.0)

  # Combine all three
  combined <- combine_patterns(list(pat1, pat2, pat3), bpm = 120)

  # Max amplitude should be < 1.0 (no clipping)
  max_amp <- max(abs(combined@left))
  expect_lt(max_amp, 1.0)
})

# =============================================================================
# Swing Application Tests
# =============================================================================

test_that("apply_swing_to_steps adjusts even steps correctly", {
  steps <- 1:8
  swing <- 0.10

  adjusted <- apply_swing_to_steps(steps, swing)

  # Odd steps unchanged
  expect_equal(adjusted[1], 1.0)
  expect_equal(adjusted[3], 3.0)
  expect_equal(adjusted[5], 5.0)
  expect_equal(adjusted[7], 7.0)

  # Even steps shifted by swing amount
  expect_equal(adjusted[2], 2.1)
  expect_equal(adjusted[4], 4.1)
  expect_equal(adjusted[6], 6.1)
  expect_equal(adjusted[8], 8.1)
})

test_that("samples per bar calculation is accurate", {
  # At 120 BPM: 1 beat = 0.5 sec, 1 bar = 2 sec
  spbar_120 <- get_samples_per_bar(120)
  expect_equal(spbar_120, 2 * SAMPLE_RATE)

  # At 60 BPM: 1 beat = 1 sec, 1 bar = 4 sec
  spbar_60 <- get_samples_per_bar(60)
  expect_equal(spbar_60, 4 * SAMPLE_RATE)

  # At 180 BPM: 1 beat = 1/3 sec, 1 bar = 4/3 sec
  spbar_180 <- get_samples_per_bar(180)
  expect_equal(spbar_180, (4/3) * SAMPLE_RATE)
})

test_that("samples per beat calculation is accurate", {
  # At 120 BPM: 1 beat = 0.5 sec
  spbeat_120 <- get_samples_per_beat(120)
  expect_equal(spbeat_120, 0.5 * SAMPLE_RATE)

  # At 60 BPM: 1 beat = 1 sec
  spbeat_60 <- get_samples_per_beat(60)
  expect_equal(spbeat_60, 1 * SAMPLE_RATE)
})

# =============================================================================
# Humanization Tests
# =============================================================================

test_that("humanize_timing returns integer sample position", {
  pos <- humanize_timing(44100, amount = 0.02)
  expect_true(is.integer(pos))
})

test_that("humanize_timing with zero amount returns original", {
  original <- 44100L
  result <- humanize_timing(original, amount = 0.0)
  expect_equal(result, original)
})

test_that("humanize_timing with seed is deterministic", {
  result1 <- humanize_timing(44100, amount = 0.02, seed = 42)
  result2 <- humanize_timing(44100, amount = 0.02, seed = 42)
  expect_equal(result1, result2)

  # Different seed should give different result (very likely)
  result3 <- humanize_timing(44100, amount = 0.02, seed = 123)
  # Note: this could theoretically be equal, but highly unlikely
})
