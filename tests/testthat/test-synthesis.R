# Test waveform synthesis functions

test_that("raver_sine generates correct length Wave", {
  duration_sec <- 1
  wave <- raver_sine(440, duration_sec)

  expect_s4_class(wave, "Wave")
  expect_equal(length(wave@left), SAMPLE_RATE * duration_sec)
})

test_that("raver_sine respects frequency parameter", {
  # Generate two different frequencies
  wave_low <- raver_sine(220, 0.1)
  wave_high <- raver_sine(440, 0.1)

  # They should have different sample values
  expect_false(all(wave_low@left == wave_high@left))
})

test_that("raver_sine respects sample_rate parameter", {
  duration_sec <- 0.5
  custom_rate <- 22050
  wave <- raver_sine(440, duration_sec, sample_rate = custom_rate)

  expect_equal(wave@samp.rate, custom_rate)
  expect_equal(length(wave@left), custom_rate * duration_sec)
})

test_that("raver_sawtooth generates different waveform than sine", {
  sine <- raver_sine(440, 0.1)
  saw <- raver_sawtooth(440, 0.1)

  # Same frequency but different samples (waveform shapes differ)
  expect_false(all(sine@left == saw@left))
})

test_that("raver_sawtooth reverse parameter works", {
  saw_normal <- raver_sawtooth(440, 0.1)
  saw_reverse <- raver_sawtooth(440, 0.1, reverse = TRUE)

  # Reversed sawtooth should be different

  expect_false(all(saw_normal@left == saw_reverse@left))
})

test_that("raver_square respects duty cycle parameter", {
  square_50 <- raver_square(440, 0.1, duty = 0.5)
  square_25 <- raver_square(440, 0.1, duty = 0.25)

  # Different duty cycles produce different waveforms
  expect_false(all(square_50@left == square_25@left))
})

test_that("raver_square default duty is 50%", {
  square_default <- raver_square(440, 0.1)
  square_50 <- raver_square(440, 0.1, duty = 0.5)

  # Default should be 50% duty cycle
  expect_equal(square_default@left, square_50@left)
})

test_that("all synthesis functions return 32-bit Wave objects", {
  sine <- raver_sine(440, 0.1)
  saw <- raver_sawtooth(440, 0.1)
  square <- raver_square(440, 0.1)

  expect_equal(sine@bit, BIT_DEPTH)
  expect_equal(saw@bit, BIT_DEPTH)
  expect_equal(square@bit, BIT_DEPTH)
})

test_that("all synthesis functions use SAMPLE_RATE by default", {
  sine <- raver_sine(440, 0.1)
  saw <- raver_sawtooth(440, 0.1)
  square <- raver_square(440, 0.1)

  expect_equal(sine@samp.rate, SAMPLE_RATE)
  expect_equal(saw@samp.rate, SAMPLE_RATE)
  expect_equal(square@samp.rate, SAMPLE_RATE)
})

test_that("all synthesis functions return mono (not stereo)", {
  sine <- raver_sine(440, 0.1)
  saw <- raver_sawtooth(440, 0.1)
  square <- raver_square(440, 0.1)

  expect_false(sine@stereo)
  expect_false(saw@stereo)
  expect_false(square@stereo)
})

test_that("create_wave creates Wave from raw samples", {
  samples <- sin(2 * pi * 440 * seq(0, 0.1, length.out = 4410))
  wave <- create_wave(samples)

  expect_s4_class(wave, "Wave")
  expect_equal(length(wave@left), length(samples))
  expect_equal(wave@bit, BIT_DEPTH)
  expect_equal(wave@samp.rate, SAMPLE_RATE)
})
