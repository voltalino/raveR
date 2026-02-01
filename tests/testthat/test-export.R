# Test audio export functions

test_that("export_wav creates file", {
  wave <- raver_sine(440, 0.1)
  temp_file <- tempfile(fileext = ".wav")
  on.exit(unlink(temp_file), add = TRUE)

  result <- export_wav(wave, temp_file)

  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)
})

test_that("export_wav creates valid WAV readable by tuneR", {
  wave <- raver_sine(440, 0.1)
  temp_file <- tempfile(fileext = ".wav")
  on.exit(unlink(temp_file), add = TRUE)

  export_wav(wave, temp_file)

  # Should be readable by tuneR
  read_back <- tuneR::readWave(temp_file)
  expect_s4_class(read_back, "Wave")
  expect_equal(read_back@samp.rate, SAMPLE_RATE)
})

test_that("export_wav adds extension if missing", {
  wave <- raver_sine(440, 0.1)
  temp_base <- tempfile()
  temp_file <- paste0(temp_base, ".wav")
  on.exit(unlink(temp_file), add = TRUE)

  result <- export_wav(wave, temp_base)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))
})

test_that("export_wav exports as 16-bit by default", {
  wave <- raver_sine(440, 0.1)
  temp_file <- tempfile(fileext = ".wav")
  on.exit(unlink(temp_file), add = TRUE)

  export_wav(wave, temp_file)

  read_back <- tuneR::readWave(temp_file)
  expect_equal(read_back@bit, EXPORT_BIT)
})

test_that("export_wav errors on non-Wave input", {
  temp_file <- tempfile(fileext = ".wav")

  expect_error(
    export_wav("not a wave", temp_file),
    "must be a tuneR Wave object"
  )
})

test_that("export_wav errors on non-existent directory", {
  wave <- raver_sine(440, 0.1)

  expect_error(
    export_wav(wave, "/nonexistent/path/file.wav"),
    "does not exist"
  )
})

test_that("check_ffmpeg returns boolean", {
  result <- suppressWarnings(check_ffmpeg())

  expect_type(result, "logical")
  expect_length(result, 1)
})

# MP3 tests - skip if FFmpeg not available
test_that("export_mp3 creates file when FFmpeg available", {
  skip_if_not(suppressWarnings(check_ffmpeg()), "FFmpeg not available")

  wave <- raver_sine(440, 0.1)
  temp_file <- tempfile(fileext = ".mp3")
  on.exit(unlink(temp_file), add = TRUE)

  result <- export_mp3(wave, temp_file)

  expect_true(file.exists(temp_file))
  expect_equal(result, temp_file)
})

test_that("export_mp3 adds extension if missing when FFmpeg available", {
  skip_if_not(suppressWarnings(check_ffmpeg()), "FFmpeg not available")

  wave <- raver_sine(440, 0.1)
  temp_base <- tempfile()
  temp_file <- paste0(temp_base, ".mp3")
  on.exit(unlink(temp_file), add = TRUE)

  result <- export_mp3(wave, temp_base)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))
})

test_that("export_mp3 respects bitrate parameter when FFmpeg available", {
  skip_if_not(suppressWarnings(check_ffmpeg()), "FFmpeg not available")

  wave <- raver_sine(440, 0.5)

  temp_low <- tempfile(fileext = ".mp3")
  temp_high <- tempfile(fileext = ".mp3")
  on.exit({
    unlink(temp_low)
    unlink(temp_high)
  }, add = TRUE)

  export_mp3(wave, temp_low, bitrate = 96000L)
  export_mp3(wave, temp_high, bitrate = 320000L)

  # Higher bitrate should produce larger file
  size_low <- file.size(temp_low)
  size_high <- file.size(temp_high)

  expect_true(size_high > size_low)
})

test_that("export_mp3 errors when FFmpeg not available", {
  skip_if(suppressWarnings(check_ffmpeg()), "FFmpeg is available (testing without FFmpeg)")

  wave <- raver_sine(440, 0.1)
  temp_file <- tempfile(fileext = ".mp3")

  expect_error(
    export_mp3(wave, temp_file),
    "FFmpeg not available"
  )
})

test_that("export_mp3 errors on non-Wave input", {
  skip_if_not(suppressWarnings(check_ffmpeg()), "FFmpeg not available")

  temp_file <- tempfile(fileext = ".mp3")

  expect_error(
    export_mp3("not a wave", temp_file),
    "must be a tuneR Wave object"
  )
})
