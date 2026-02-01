# Test buffer architecture functions

test_that("create_silence generates correct duration", {
  duration_sec <- 0.5
  silence <- create_silence(duration_sec)

  expect_s4_class(silence, "Wave")
  expect_equal(length(silence@left), SAMPLE_RATE * duration_sec)
})

test_that("create_silence returns 32-bit Wave", {
  silence <- create_silence(0.1)

  expect_equal(silence@bit, BIT_DEPTH)
  expect_false(silence@pcm)  # PCM_MODE is FALSE for 32-bit float
})

test_that("create_silence samples are zero", {
  silence <- create_silence(0.1)

  expect_true(all(silence@left == 0))
})

test_that("create_silence respects sample_rate parameter", {
  custom_rate <- 22050
  duration_sec <- 0.5
  silence <- create_silence(duration_sec, sample_rate = custom_rate)

  expect_equal(silence@samp.rate, custom_rate)
  expect_equal(length(silence@left), custom_rate * duration_sec)
})

test_that("render_buffer calls content_fn", {
  call_count <- 0

  buffer <- render_buffer(1, function() {
    call_count <<- call_count + 1
    raver_sine(440, 1)
  })

  expect_equal(call_count, 1)
})

test_that("render_buffer returns Wave from content_fn", {
  buffer <- render_buffer(1, function() raver_sine(440, 1))

  expect_s4_class(buffer, "Wave")
  expect_equal(length(buffer@left), SAMPLE_RATE * 1)
})

test_that("render_buffer errors on non-Wave content", {
  expect_error(
    render_buffer(1, function() "not a wave"),
    "must return a tuneR Wave object"
  )
})

test_that("normalize_for_export converts to 16-bit", {
  wave_32 <- raver_sine(440, 0.1)
  expect_equal(wave_32@bit, 32)

  wave_16 <- normalize_for_export(wave_32)
  expect_equal(wave_16@bit, 16)
})

test_that("normalize_for_export returns valid PCM values", {
  wave_32 <- raver_sine(440, 0.1)
  wave_16 <- normalize_for_export(wave_32)

  # 16-bit PCM values should be in range
  expect_true(all(wave_16@left >= -32768))
  expect_true(all(wave_16@left <= 32767))
})

test_that("normalize_for_export errors on non-Wave input", {
  expect_error(
    normalize_for_export("not a wave"),
    "must be a tuneR Wave object"
  )
})

test_that("bind_waves concatenates correctly", {
  w1 <- raver_sine(440, 0.1)
  w2 <- raver_sine(550, 0.1)

  combined <- bind_waves(w1, w2)

  expect_s4_class(combined, "Wave")
  expect_equal(length(combined@left), length(w1@left) + length(w2@left))
})

test_that("bind_waves preserves wave format", {
  w1 <- raver_sine(440, 0.1)
  w2 <- raver_sine(550, 0.1)

  combined <- bind_waves(w1, w2)

  expect_equal(combined@samp.rate, SAMPLE_RATE)
  expect_equal(combined@bit, BIT_DEPTH)
  expect_false(combined@stereo)
})

test_that("bind_waves works with three or more waves", {
  w1 <- raver_sine(440, 0.1)
  w2 <- raver_sine(550, 0.1)
  w3 <- raver_sine(660, 0.1)

  combined <- bind_waves(w1, w2, w3)

  expected_length <- length(w1@left) + length(w2@left) + length(w3@left)
  expect_equal(length(combined@left), expected_length)
})

test_that("bind_waves errors on mismatched sample rates", {
  w1 <- raver_sine(440, 0.1, sample_rate = 44100)
  w2 <- raver_sine(440, 0.1, sample_rate = 22050)

  expect_error(
    bind_waves(w1, w2),
    "Sample rate mismatch"
  )
})

test_that("bind_waves errors on empty input", {
  expect_error(
    bind_waves(),
    "At least one Wave object"
  )
})

test_that("bind_waves errors on non-Wave argument", {
  w1 <- raver_sine(440, 0.1)

  expect_error(
    bind_waves(w1, "not a wave"),
    "is not a Wave object"
  )
})
