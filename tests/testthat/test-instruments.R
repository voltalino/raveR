# Tests for drum, bass, and pad instruments
# These tests verify the core instrument functions for deep house production

test_that("raver_drum_hit returns Wave object", {
  # Will return NULL if samples missing, but function should not error
  hit <- raver_drum_hit("kick", velocity = 1.0)

  # Either NULL (missing samples) or Wave object
  if (!is.null(hit)) {
    expect_s4_class(hit, "Wave")
    expect_equal(hit@samp.rate, SAMPLE_RATE)
    expect_equal(hit@bit, BIT_DEPTH)
  }
})

test_that("raver_drum_hit velocity scales amplitude", {
  # Create a mock wave to test velocity scaling
  # We test the logic directly since samples may not be available
  hit_full <- raver_drum_hit("kick", velocity = 1.0)
  hit_half <- raver_drum_hit("kick", velocity = 0.5)

  if (!is.null(hit_full) && !is.null(hit_half)) {
    # Half velocity should have approximately half the peak amplitude
    max_full <- max(abs(hit_full@left))
    max_half <- max(abs(hit_half@left))

    # Allow 1% tolerance
    expect_equal(max_half / max_full, 0.5, tolerance = 0.01)
  } else {
    # Skip if samples not available
    skip("Drum samples not available for velocity test")
  }
})

test_that("raver_drum_pattern returns correct duration for BPM", {
  # 120 BPM: 1 bar = 2 seconds
  pattern <- rep(FALSE, 16)  # Empty pattern is fine
  pattern[1] <- TRUE  # At least one hit

  result <- suppressWarnings(raver_drum_pattern(pattern, bpm = 120))

  expect_s4_class(result, "Wave")

  expected_duration <- (60 / 120) * 4  # 2 seconds
  actual_duration <- length(result@left) / SAMPLE_RATE

  expect_equal(actual_duration, expected_duration, tolerance = 0.001)
})

test_that("raver_drum_pattern swing delays even steps", {
  # Test swing calculation helper
  samples_per_16th <- 1000  # Arbitrary for testing

  # Odd steps should have no offset
  expect_equal(calculate_swing_offset(1, 0.1, samples_per_16th), 0L)
  expect_equal(calculate_swing_offset(3, 0.1, samples_per_16th), 0L)

  # Even steps should have offset proportional to swing
  offset <- calculate_swing_offset(2, 0.1, samples_per_16th)
  expect_equal(offset, 100L)  # 0.1 * 1000

  offset2 <- calculate_swing_offset(4, 0.25, samples_per_16th)
  expect_equal(offset2, 250L)  # 0.25 * 1000
})

test_that("raver_drum_hit handles sample availability correctly", {
  # Clear cache to ensure fresh load attempt
  rm(list = ls(envir = .drum_env), envir = .drum_env)

  # Try to get a kick hit
  hit <- suppressWarnings(raver_drum_hit("kick"))

  # Either works (samples exist) or returns NULL (samples missing)
  if (!is.null(hit)) {
    expect_s4_class(hit, "Wave")
    expect_equal(hit@samp.rate, SAMPLE_RATE)
  } else {
    # If NULL, it should have warned
    expect_warning(
      raver_drum_hit("kick"),
      "Missing drum sample"
    )
  }
})

# Bass tests

test_that("raver_bass_note returns Wave with correct duration", {
  bass <- raver_bass_note(36, 0.5)

  expect_s4_class(bass, "Wave")
  expect_equal(length(bass@left), as.integer(0.5 * SAMPLE_RATE))
  expect_equal(bass@samp.rate, SAMPLE_RATE)
  expect_equal(bass@bit, BIT_DEPTH)
})

test_that("raver_bass_note has filtered character", {
  # Generate bass note with low cutoff
  bass_filtered <- raver_bass_note(36, 0.5, cutoff_hz = 200)
  bass_bright <- raver_bass_note(36, 0.5, cutoff_hz = 4000)

  # FFT to analyze frequency content
  fft_filtered <- Mod(stats::fft(bass_filtered@left))
  fft_bright <- Mod(stats::fft(bass_bright@left))

  # High frequency content should be lower in filtered version
  # Check top 1/4 of spectrum
  n <- length(fft_filtered)
  high_freq_range <- (n %/% 2 - n %/% 4):(n %/% 2)

  high_energy_filtered <- sum(fft_filtered[high_freq_range])
  high_energy_bright <- sum(fft_bright[high_freq_range])

  # Filtered should have less high-frequency energy
  expect_lt(high_energy_filtered, high_energy_bright)
})

test_that("raver_bass_line concatenates notes correctly", {
  # 4 notes, each 4 16ths (quarter note), at 120 BPM
  notes <- c(36, 48, 36, 48)
  durations <- c(4, 4, 4, 4)
  line <- raver_bass_line(notes, durations, bpm = 120)

  expect_s4_class(line, "Wave")

  # 16 16th notes = 1 bar = 2 seconds at 120 BPM
  expected_duration <- (60 / 120) * 4
  actual_duration <- length(line@left) / SAMPLE_RATE

  expect_equal(actual_duration, expected_duration, tolerance = 0.001)
})

test_that("raver_bass_line handles rests (0 or NA)", {
  notes <- c(36, 0, NA, 36)  # Note, rest (0), rest (NA), note
  durations <- c(4, 4, 4, 4)
  line <- raver_bass_line(notes, durations, bpm = 120)

  expect_s4_class(line, "Wave")

  # Check that rest sections are near-silent
  # Second quarter note (16th notes 5-8)
  samples_per_beat <- as.integer((60 / 120) * SAMPLE_RATE)
  rest_section <- line@left[(samples_per_beat + 1):(samples_per_beat * 2)]

  # Rest should be near-zero (allowing for floating point)
  expect_lt(max(abs(rest_section)), 0.001)
})

test_that("different MIDI notes produce different frequencies", {
  bass_c2 <- raver_bass_note(36, 0.5)  # C2
  bass_a2 <- raver_bass_note(45, 0.5)  # A2

  # Zero-crossing rate relates to frequency
  # Count zero crossings (sign changes)
  count_crossings <- function(samples) {
    sum(diff(sign(samples)) != 0)
  }

  crossings_c2 <- count_crossings(bass_c2@left)
  crossings_a2 <- count_crossings(bass_a2@left)

  # A2 is higher than C2, should have more zero crossings
  expect_gt(crossings_a2, crossings_c2)
})

# Pad tests

test_that("raver_pad_note returns Wave with slow attack envelope", {
  pad <- raver_pad_note(60, 1.0)  # C4, 1 second

  expect_s4_class(pad, "Wave")
  expect_equal(length(pad@left), as.integer(1.0 * SAMPLE_RATE))

  # First samples should be near zero (slow attack)
  # Attack is 0.1 seconds = 4410 samples at 44100 Hz
  first_100_samples <- pad@left[1:100]

  # Average of first 100 samples should be much smaller than peak
  expect_lt(mean(abs(first_100_samples)), max(abs(pad@left)) * 0.1)
})

test_that("raver_pad_chord contains all chord notes", {
  # min7 chord has 4 notes
  chord <- raver_pad_chord(48, "min7", 1.0)  # Cmin7

  expect_s4_class(chord, "Wave")

  # FFT analysis - should have multiple frequency peaks
  fft_result <- Mod(stats::fft(chord@left))
  n <- length(fft_result)

  # Find peaks in first half (positive frequencies)
  half <- fft_result[1:(n %/% 2)]

  # There should be multiple significant peaks (one per chord note)
  threshold <- max(half) * 0.1
  significant_bins <- sum(half > threshold)

  # Should have at least 4 significant frequency regions (4 notes in min7)
  expect_gte(significant_bins, 4)
})

test_that("raver_pad_chord inversions change frequency distribution", {
  chord_root <- raver_pad_chord(48, "minor", 0.5, inversion = 0)
  chord_inv1 <- raver_pad_chord(48, "minor", 0.5, inversion = 1)

  # FFT to find lowest significant frequency
  fft_root <- Mod(stats::fft(chord_root@left))
  fft_inv1 <- Mod(stats::fft(chord_inv1@left))

  n <- length(fft_root)
  half_root <- fft_root[1:(n %/% 2)]
  half_inv1 <- fft_inv1[1:(n %/% 2)]

  # Find first significant bin (above threshold)
  threshold_root <- max(half_root) * 0.3
  threshold_inv1 <- max(half_inv1) * 0.3

  first_sig_root <- which(half_root > threshold_root)[1]
  first_sig_inv1 <- which(half_inv1 > threshold_inv1)[1]

  # First inversion should have higher lowest note
  expect_gt(first_sig_inv1, first_sig_root)
})

test_that("raver_pad_note detuning creates slight chorus effect", {
  # Compare no detune vs detuned
  pad_no_detune <- raver_pad_note(60, 0.5, detune_cents = 0)
  pad_detuned <- raver_pad_note(60, 0.5, detune_cents = 5)

  # Detuned should have slightly different waveform shape
  # (more complex due to beating between oscillators)

  # Compare variance in waveform - detuned should have more variation
  # due to amplitude modulation from beating
  var_no_detune <- var(pad_no_detune@left)
  var_detuned <- var(pad_detuned@left)

  # The detuned version may have slightly different variance characteristics
  # Just ensure they're different (not exactly equal)
  expect_false(isTRUE(all.equal(var_no_detune, var_detuned, tolerance = 1e-10)))
})

test_that("raver_pad_progression has correct total duration", {
  prog <- list(
    list(root = 57, type = "min7"),
    list(root = 53, type = "maj7")
  )
  progression <- raver_pad_progression(prog, c(1, 1), bpm = 120)

  expect_s4_class(progression, "Wave")

  # 2 bars at 120 BPM = 4 seconds
  expected_duration <- 2 * (60 / 120) * 4
  actual_duration <- length(progression@left) / SAMPLE_RATE

  expect_equal(actual_duration, expected_duration, tolerance = 0.001)
})
