# test-music-theory.R
# Unit tests for music theory functions

# =============================================================================
# MIDI-to-Frequency Conversion Tests
# =============================================================================

test_that("MIDI 69 (A4) converts to exactly 440 Hz", {
  expect_equal(raver_midi_to_freq(69), 440)
})

test_that("MIDI 60 (C4) converts to ~261.63 Hz", {
  freq <- raver_midi_to_freq(60)
  expect_true(abs(freq - 261.626) < 0.01)
})

test_that("MIDI 57 (A3) converts to 220 Hz", {
  expect_equal(raver_midi_to_freq(57), 220)
})

test_that("octave relationship: note + 12 = double frequency", {
  freq_a3 <- raver_midi_to_freq(57)
  freq_a4 <- raver_midi_to_freq(69)
  expect_equal(freq_a4, freq_a3 * 2)

  freq_c4 <- raver_midi_to_freq(60)
  freq_c5 <- raver_midi_to_freq(72)
  expect_equal(freq_c5, freq_c4 * 2)
})

test_that("edge case: very low and high MIDI notes", {
  # MIDI 0 (C-1) should produce a very low frequency
  freq_low <- raver_midi_to_freq(0)
  expect_true(freq_low > 0)
  expect_true(freq_low < 20)  # Below human hearing

  # MIDI 127 (G9) should produce a very high frequency
  freq_high <- raver_midi_to_freq(127)
  expect_true(freq_high > 10000)
})

# =============================================================================
# Note-to-MIDI Conversion Tests
# =============================================================================

test_that("natural notes in octave 4 convert correctly", {
  expect_equal(raver_note_to_midi("C", 4), 60)
  expect_equal(raver_note_to_midi("D", 4), 62)
  expect_equal(raver_note_to_midi("E", 4), 64)
  expect_equal(raver_note_to_midi("F", 4), 65)
  expect_equal(raver_note_to_midi("G", 4), 67)
  expect_equal(raver_note_to_midi("A", 4), 69)
  expect_equal(raver_note_to_midi("B", 4), 71)
})

test_that("sharps convert correctly", {
  expect_equal(raver_note_to_midi("C#", 4), 61)
  expect_equal(raver_note_to_midi("Cs", 4), 61)
  expect_equal(raver_note_to_midi("F#", 4), 66)
  expect_equal(raver_note_to_midi("Fs", 4), 66)
})

test_that("flats convert correctly", {
  expect_equal(raver_note_to_midi("Bb", 4), 70)
  expect_equal(raver_note_to_midi("Bf", 4), 70)
  expect_equal(raver_note_to_midi("Eb", 4), 63)
  expect_equal(raver_note_to_midi("Ef", 4), 63)
})

test_that("octave boundaries are correct", {
  expect_equal(raver_note_to_midi("C", 0), 12)
  expect_equal(raver_note_to_midi("C", 8), 108)
  expect_equal(raver_note_to_midi("C", -1), 0)  # Lowest MIDI note
})

test_that("note names are case insensitive", {
  expect_equal(raver_note_to_midi("c", 4), 60)
  expect_equal(raver_note_to_midi("a", 4), 69)
  expect_equal(raver_note_to_midi("c#", 4), 61)
  expect_equal(raver_note_to_midi("bb", 4), 70)
})

# =============================================================================
# Frequency-to-MIDI Conversion Tests
# =============================================================================

test_that("freq_to_midi reverses midi_to_freq", {
  expect_equal(raver_freq_to_midi(440), 69)
  expect_equal(raver_freq_to_midi(261.63), 60)
  expect_equal(raver_freq_to_midi(220), 57)
})

# =============================================================================
# Scale Construction Tests
# =============================================================================

test_that("A minor scale contains correct notes", {
  scale <- raver_build_scale(57, "natural_minor", 1)
  # A minor: A, B, C, D, E, F, G (MIDI: 57, 59, 60, 62, 64, 65, 67)
  expected <- c(57L, 59L, 60L, 62L, 64L, 65L, 67L)
  expect_equal(scale, expected)
})

test_that("C major scale contains correct notes", {
  scale <- raver_build_scale(60, "major", 1)
  # C major: C, D, E, F, G, A, B (MIDI: 60, 62, 64, 65, 67, 69, 71)
  expected <- c(60L, 62L, 64L, 65L, 67L, 69L, 71L)
  expect_equal(scale, expected)
})

test_that("multi-octave scales have correct length", {
  scale_1oct <- raver_build_scale(60, "major", 1)
  scale_2oct <- raver_build_scale(60, "major", 2)
  scale_3oct <- raver_build_scale(60, "major", 3)

  expect_equal(length(scale_1oct), 7)
  expect_equal(length(scale_2oct), 14)
  expect_equal(length(scale_3oct), 21)
})

test_that("Dorian mode intervals are correct", {
  scale <- raver_build_scale(62, "dorian", 1)
  # D Dorian: D, E, F, G, A, B, C (same notes as C major but from D)
  # MIDI: 62, 64, 65, 67, 69, 71, 72
  expected <- c(62L, 64L, 65L, 67L, 69L, 71L, 72L)
  expect_equal(scale, expected)
})

test_that("harmonic minor has raised 7th", {
  # A harmonic minor: A, B, C, D, E, F, G#
  scale <- raver_build_scale(57, "harmonic_minor", 1)
  expected <- c(57L, 59L, 60L, 62L, 64L, 65L, 68L)  # G# = 68 instead of G = 67
  expect_equal(scale, expected)
})

# =============================================================================
# Chord Construction Tests
# =============================================================================

test_that("major triad has correct intervals (0, 4, 7)", {
  chord <- raver_build_chord(60, "major")
  # C major: C, E, G (MIDI: 60, 64, 67)
  expect_equal(chord, c(60L, 64L, 67L))
})

test_that("minor 7th has correct intervals (0, 3, 7, 10)", {
  chord <- raver_build_chord(57, "min7")
  # A min7: A, C, E, G (MIDI: 57, 60, 64, 67)
  expect_equal(chord, c(57L, 60L, 64L, 67L))
})

test_that("min9 chord has correct intervals (0, 3, 7, 10, 14)", {
  chord <- raver_build_chord(48, "min9")
  # C min9: C, Eb, G, Bb, D (MIDI: 48, 51, 55, 58, 62)
  expect_equal(chord, c(48L, 51L, 55L, 58L, 62L))
})

test_that("chord inversions move correct notes up an octave", {
  # Root position C minor: C, Eb, G (48, 51, 55)
  root <- c(48L, 51L, 55L)

  # First inversion: Eb, G, C (51, 55, 60)
  inv1 <- raver_invert_chord(root, 1)
  expect_equal(inv1, c(51L, 55L, 60L))

  # Second inversion: G, C, Eb (55, 60, 63)
  inv2 <- raver_invert_chord(root, 2)
  expect_equal(inv2, c(55L, 60L, 63L))

  # Third inversion wraps to root position
  inv3 <- raver_invert_chord(root, 3)
  expect_equal(inv3, root)
})

test_that("get_chord_freqs returns frequencies", {
  freqs <- raver_get_chord_freqs(69, "major")
  # A major: A4, C#5, E5
  expect_equal(freqs[1], 440)  # A4 = 440 Hz
  expect_true(length(freqs) == 3)
  expect_true(all(freqs > 0))
})

# =============================================================================
# Determinism Tests
# =============================================================================

test_that("same scale call twice produces identical result", {
  scale1 <- raver_build_scale(57, "natural_minor", 2)
  scale2 <- raver_build_scale(57, "natural_minor", 2)
  expect_identical(scale1, scale2)
})

test_that("same chord call twice produces identical result", {
  chord1 <- raver_build_chord(48, "min9")
  chord2 <- raver_build_chord(48, "min9")
  expect_identical(chord1, chord2)
})

# =============================================================================
# Constants Tests
# =============================================================================

test_that("SCALE_PATTERNS contains expected patterns", {
  expect_true("major" %in% names(SCALE_PATTERNS))
  expect_true("natural_minor" %in% names(SCALE_PATTERNS))
  expect_true("harmonic_minor" %in% names(SCALE_PATTERNS))
  expect_true("dorian" %in% names(SCALE_PATTERNS))
})

test_that("CHORD_FORMULAS contains expected chord types", {
  expect_true("major" %in% names(CHORD_FORMULAS))
  expect_true("minor" %in% names(CHORD_FORMULAS))
  expect_true("maj7" %in% names(CHORD_FORMULAS))
  expect_true("min7" %in% names(CHORD_FORMULAS))
  expect_true("dom7" %in% names(CHORD_FORMULAS))
  expect_true("min9" %in% names(CHORD_FORMULAS))
})

test_that("HOUSE_KEYS contains common keys", {
  expect_true("A_minor" %in% names(HOUSE_KEYS))
  expect_true("C_major" %in% names(HOUSE_KEYS))
  expect_equal(HOUSE_KEYS[["A_minor"]], 57L)
  expect_equal(HOUSE_KEYS[["C_major"]], 48L)
})

# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("invalid note name raises error", {
  expect_error(raver_note_to_midi("X", 4), "Invalid note name")
  expect_error(raver_note_to_midi("H", 4), "Invalid note name")
})

test_that("invalid scale pattern raises error", {
  expect_error(raver_build_scale(60, "invalid_pattern"), "Unknown scale pattern")
})

test_that("invalid chord type raises error", {
  expect_error(raver_build_chord(60, "invalid_chord"), "Unknown chord type")
})
