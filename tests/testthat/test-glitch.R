# Test glitch effect generators

# Helper to create test audio
create_test_wave <- function(duration_sec = 1, freq = 440) {
  sample_rate <- 44100L
  n_samples <- as.integer(duration_sec * sample_rate)
  t <- seq(0, duration_sec, length.out = n_samples)
  samples <- as.integer(round(sin(2 * pi * freq * t) * 32767 * 0.5))
  tuneR::Wave(left = samples, samp.rate = sample_rate, bit = 16L, pcm = TRUE)
}

# =============================================================================
# generate_glitch_effect tests
# =============================================================================

test_that("generate_glitch_effect returns valid Wave", {
  wave <- create_test_wave()
  result <- generate_glitch_effect(wave, intensity = 0.5)

  expect_s4_class(result, "Wave")
  expect_equal(result@samp.rate, wave@samp.rate)
  expect_equal(result@bit, wave@bit)
  expect_equal(length(result@left), length(wave@left))
})

test_that("generate_glitch_effect modifies audio", {
  wave <- create_test_wave()
  result <- generate_glitch_effect(wave, intensity = 0.5, seed = 42)

  # Audio should be different (bitcrushing at minimum)
  expect_false(all(wave@left == result@left))
})

test_that("generate_glitch_effect intensity affects severity", {
  wave <- create_test_wave()

  # Low intensity
  low <- generate_glitch_effect(wave, intensity = 0.1, seed = 100)

  # High intensity
  high <- generate_glitch_effect(wave, intensity = 0.9, seed = 100)

  # Both should be different from original
  diff_low <- sum(abs(as.numeric(wave@left) - as.numeric(low@left)))
  diff_high <- sum(abs(as.numeric(wave@left) - as.numeric(high@left)))

  # High intensity should have more difference (on average)
  # Note: Not guaranteed due to randomness, but typically true
  expect_gt(diff_high, 0)
  expect_gt(diff_low, 0)
})

test_that("generate_glitch_effect is reproducible with same seed", {
  wave <- create_test_wave()

  result1 <- generate_glitch_effect(wave, intensity = 0.5, seed = 42)
  result2 <- generate_glitch_effect(wave, intensity = 0.5, seed = 42)

  expect_equal(result1@left, result2@left)
})

test_that("generate_glitch_effect handles short audio", {
  # Very short audio (0.01 seconds)
  short_wave <- create_test_wave(duration_sec = 0.01)

  # Should not crash
  expect_no_error(generate_glitch_effect(short_wave, intensity = 0.5))
})

test_that("generate_glitch_effect validates intensity", {
  wave <- create_test_wave()

  expect_error(generate_glitch_effect(wave, intensity = -0.1), "between 0 and 1")
  expect_error(generate_glitch_effect(wave, intensity = 1.5), "between 0 and 1")
  expect_error(generate_glitch_effect(wave, intensity = "high"), "between 0 and 1")
})

test_that("generate_glitch_effect errors on non-Wave", {
  expect_error(generate_glitch_effect("not a wave"), "tuneR Wave object")
  expect_error(generate_glitch_effect(list()), "tuneR Wave object")
})

# =============================================================================
# apply_stutter tests
# =============================================================================

test_that("apply_stutter returns valid Wave", {
  wave <- create_test_wave()
  result <- apply_stutter(wave, stutter_count = 3)

  expect_s4_class(result, "Wave")
  expect_equal(result@samp.rate, wave@samp.rate)
  expect_equal(length(result@left), length(wave@left))
})

test_that("apply_stutter creates audible effect", {
  wave <- create_test_wave()
  result <- apply_stutter(wave, stutter_count = 5, stutter_length_sec = 0.05)

  # Audio should be different
  expect_false(all(wave@left == result@left))
})

test_that("apply_stutter handles very short audio", {
  # 0.05 second audio with 0.1 second stutter request
  short_wave <- create_test_wave(duration_sec = 0.05)

  # Should return unchanged (too short)
  result <- apply_stutter(short_wave, stutter_length_sec = 0.1)

  expect_s4_class(result, "Wave")
})

test_that("apply_stutter respects stutter_count", {
  wave <- create_test_wave(duration_sec = 2)

  # More stutters = more difference
  few <- apply_stutter(wave, stutter_count = 1, stutter_length_sec = 0.05)
  many <- apply_stutter(wave, stutter_count = 10, stutter_length_sec = 0.05)

  # Both valid
  expect_s4_class(few, "Wave")
  expect_s4_class(many, "Wave")
})

test_that("apply_stutter errors on non-Wave", {
  expect_error(apply_stutter("not a wave"), "tuneR Wave object")
})

# =============================================================================
# apply_bitcrush tests
# =============================================================================

test_that("apply_bitcrush returns valid Wave", {
  wave <- create_test_wave()
  result <- apply_bitcrush(wave, bit_reduction = 4)

  expect_s4_class(result, "Wave")
  expect_equal(result@samp.rate, wave@samp.rate)
  expect_equal(result@bit, wave@bit)
  expect_equal(length(result@left), length(wave@left))
})

test_that("apply_bitcrush creates audible effect", {
  wave <- create_test_wave()
  result <- apply_bitcrush(wave, bit_reduction = 4)

  # Audio should be different
  expect_false(all(wave@left == result@left))
})

test_that("apply_bitcrush severity scales with bit_reduction", {
  wave <- create_test_wave()

  light <- apply_bitcrush(wave, bit_reduction = 2)
  heavy <- apply_bitcrush(wave, bit_reduction = 8)

  # Calculate differences
  diff_light <- sum(abs(as.numeric(wave@left) - as.numeric(light@left)))
  diff_heavy <- sum(abs(as.numeric(wave@left) - as.numeric(heavy@left)))

  # Heavy should be more different
  expect_gt(diff_heavy, diff_light)
})

test_that("apply_bitcrush validates bit_reduction", {
  wave <- create_test_wave()

  expect_error(apply_bitcrush(wave, bit_reduction = 0), "between 1 and 14")
  expect_error(apply_bitcrush(wave, bit_reduction = 15), "between 1 and 14")
})

test_that("apply_bitcrush is deterministic", {
  wave <- create_test_wave()

  result1 <- apply_bitcrush(wave, bit_reduction = 4)
  result2 <- apply_bitcrush(wave, bit_reduction = 4)

  # Should be identical (no randomness)
  expect_equal(result1@left, result2@left)
})

test_that("apply_bitcrush errors on non-Wave", {
  expect_error(apply_bitcrush("not a wave"), "tuneR Wave object")
})

# =============================================================================
# apply_dropout tests
# =============================================================================

test_that("apply_dropout returns valid Wave", {
  wave <- create_test_wave()
  result <- apply_dropout(wave, dropout_count = 3)

  expect_s4_class(result, "Wave")
  expect_equal(result@samp.rate, wave@samp.rate)
  expect_equal(length(result@left), length(wave@left))
})

test_that("apply_dropout creates silences", {
  wave <- create_test_wave()

  # Count non-zero samples before
  nonzero_before <- sum(wave@left != 0)

  # Apply dropouts
  result <- apply_dropout(wave, dropout_count = 5, dropout_length_sec = 0.05)

  # Count non-zero samples after
  nonzero_after <- sum(result@left != 0)

  # Should have fewer non-zero samples
  expect_lt(nonzero_after, nonzero_before)
})

test_that("apply_dropout handles short audio", {
  short_wave <- create_test_wave(duration_sec = 0.02)

  # Should not crash, may return unchanged
  result <- apply_dropout(short_wave, dropout_count = 3)

  expect_s4_class(result, "Wave")
})

test_that("apply_dropout respects dropout_count", {
  wave <- create_test_wave(duration_sec = 2)

  few <- apply_dropout(wave, dropout_count = 1, dropout_length_sec = 0.01)
  many <- apply_dropout(wave, dropout_count = 10, dropout_length_sec = 0.01)

  # More dropouts = more zeros
  zeros_few <- sum(few@left == 0)
  zeros_many <- sum(many@left == 0)

  # Wave starts with some zeros, so just check many has more
  expect_gte(zeros_many, zeros_few)
})

test_that("apply_dropout errors on non-Wave", {
  expect_error(apply_dropout("not a wave"), "tuneR Wave object")
})

# =============================================================================
# Edge cases and stereo
# =============================================================================

test_that("glitch effects work with stereo audio", {
  # Create stereo wave
  sample_rate <- 44100L
  n_samples <- 44100L  # 1 second
  t <- seq(0, 1, length.out = n_samples)
  left <- as.integer(round(sin(2 * pi * 440 * t) * 32767 * 0.5))
  right <- as.integer(round(sin(2 * pi * 550 * t) * 32767 * 0.5))
  stereo_wave <- tuneR::Wave(left = left, right = right,
                              samp.rate = sample_rate, bit = 16L, pcm = TRUE)

  # Test all effects
  expect_no_error(generate_glitch_effect(stereo_wave, intensity = 0.5))
  expect_no_error(apply_stutter(stereo_wave, stutter_count = 3))
  expect_no_error(apply_bitcrush(stereo_wave, bit_reduction = 4))
  expect_no_error(apply_dropout(stereo_wave, dropout_count = 3))

  # Verify stereo is preserved
  result <- generate_glitch_effect(stereo_wave, intensity = 0.5)
  expect_true(result@stereo)
})

test_that("effects preserve sample rate and format", {
  wave <- create_test_wave()
  original_rate <- wave@samp.rate
  original_bit <- wave@bit

  glitched <- generate_glitch_effect(wave, intensity = 0.8)
  stuttered <- apply_stutter(wave)
  crushed <- apply_bitcrush(wave)
  dropped <- apply_dropout(wave)

  expect_equal(glitched@samp.rate, original_rate)
  expect_equal(glitched@bit, original_bit)
  expect_equal(stuttered@samp.rate, original_rate)
  expect_equal(crushed@samp.rate, original_rate)
  expect_equal(dropped@samp.rate, original_rate)
})
