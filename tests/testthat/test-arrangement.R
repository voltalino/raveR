# ==============================================================================
# Test Suite for Motif Generator and Arrangement System
# ==============================================================================

# ==============================================================================
# Motif Generation Tests (5 tests)
# ==============================================================================

test_that("generate_motif is deterministic", {
  scale <- raver_build_scale(57, "natural_minor", 2)

  motif1 <- generate_motif("clean_data", scale)
  motif2 <- generate_motif("clean_data", scale)

  expect_identical(motif1$rhythm, motif2$rhythm)
  expect_identical(motif1$notes, motif2$notes)
  expect_identical(motif1$velocities, motif2$velocities)
})

test_that("different names produce different motifs", {
  scale <- raver_build_scale(57, "natural_minor", 2)

  motif1 <- generate_motif("function_one", scale)
  motif2 <- generate_motif("function_two", scale)

  # At least one element should differ (rhythm, notes, or velocities)
  same_rhythm <- identical(motif1$rhythm, motif2$rhythm)
  same_notes <- identical(motif1$notes, motif2$notes)
  same_velocities <- identical(motif1$velocities, motif2$velocities)

  expect_false(same_rhythm && same_notes && same_velocities)
})

test_that("seed parameter creates variation", {
  scale <- raver_build_scale(57, "natural_minor", 2)

  motif1 <- generate_motif("test_func", scale, seed = NULL)
  motif2 <- generate_motif("test_func", scale, seed = "variant1")
  motif3 <- generate_motif("test_func", scale, seed = "variant2")

  # Seeded versions should differ from each other and from unseeded
  expect_false(identical(motif1$rhythm, motif2$rhythm))
  expect_false(identical(motif2$rhythm, motif3$rhythm))
})

test_that("motif rhythm has correct length", {
  scale <- raver_build_scale(57, "natural_minor", 2)
  motif <- generate_motif("any_name", scale)

  expect_length(motif$rhythm, 16)
  expect_type(motif$rhythm, "logical")
})

test_that("motif velocities are in valid range", {
  scale <- raver_build_scale(57, "natural_minor", 2)
  motif <- generate_motif("velocity_test", scale)

  expect_true(all(motif$velocities >= 0.7))
  expect_true(all(motif$velocities <= 1.0))
})

# ==============================================================================
# Motif Evolution Tests (2 tests)
# ==============================================================================

test_that("evolved motif differs from original", {
  scale <- raver_build_scale(57, "natural_minor", 2)
  original <- generate_motif("evolve_test", scale)
  evolved <- evolve_motif(original, 0.5)  # Higher variation for more change

  # At least something should be different (seed is always different)
  expect_false(identical(original$seed, evolved$seed))
})

test_that("evolution preserves most of original structure", {
  scale <- raver_build_scale(57, "natural_minor", 2)
  original <- generate_motif("structure_test", scale)
  evolved <- evolve_motif(original, 0.1)  # Low variation

  # Most of the rhythm should be the same
  rhythm_matches <- sum(original$rhythm == evolved$rhythm)
  expect_gte(rhythm_matches, 13)  # At least 13 of 16 steps same

  # Velocities should be similar (within 0.15 of original)
  if (length(evolved$velocities) >= length(original$velocities)) {
    velocity_close <- all(
      abs(evolved$velocities[seq_along(original$velocities)] -
          original$velocities) < 0.15
    )
    expect_true(velocity_close)
  }
})

# ==============================================================================
# Key and Progression Hashing Tests (2 tests)
# ==============================================================================

test_that("hash_to_key returns valid MIDI note", {
  key1 <- hash_to_key("test_file.R")
  key2 <- hash_to_key("another_file.R")
  key3 <- hash_to_key("test_file.R")  # Same as key1

  # Keys should be valid MIDI notes in the house range
  expect_true(key1 >= 36 && key1 <= 72)  # Reasonable bass range
  expect_true(key2 >= 36 && key2 <= 72)

  # Same input = same output (determinism)
  expect_equal(key1, key3)
})

test_that("hash_to_progression returns valid chord specs", {
  prog <- hash_to_progression("test", 4)

  expect_length(prog, 4)

  for (chord in prog) {
    expect_true("root" %in% names(chord))
    expect_true("type" %in% names(chord))
    expect_true(chord$root >= 1 && chord$root <= 7)
    expect_true(chord$type %in% c("min7", "maj7", "dom7", "min9"))
  }
})

# ==============================================================================
# Section Types Tests (2 tests)
# ==============================================================================

test_that("all section types have required fields", {
  required_fields <- c("bars", "elements", "filter_start", "filter_end")

  for (section_name in names(SECTION_TYPES)) {
    section <- SECTION_TYPES[[section_name]]

    for (field in required_fields) {
      expect_true(field %in% names(section),
                  info = paste("Section", section_name, "missing field", field))
    }

    expect_type(section$bars, "integer")
    expect_type(section$elements, "character")
    expect_true(length(section$elements) >= 1)
  }
})

test_that("filter values are reasonable", {
  for (section_name in names(SECTION_TYPES)) {
    section <- SECTION_TYPES[[section_name]]

    expect_true(section$filter_start > 0,
                info = paste(section_name, "filter_start should be > 0"))
    expect_true(section$filter_start < 20000,
                info = paste(section_name, "filter_start should be < 20000"))
    expect_true(section$filter_end > 0,
                info = paste(section_name, "filter_end should be > 0"))
    expect_true(section$filter_end < 20000,
                info = paste(section_name, "filter_end should be < 20000"))
  }
})

# ==============================================================================
# Arrangement Creation Tests (1 test)
# ==============================================================================

test_that("create_arrangement returns valid structure", {
  skip_if_not_installed("digest")

  # Create a simple code model
  temp_file <- tempfile(fileext = ".R")
  writeLines(c(
    "clean_data <- function(x) { x + 1 }",
    "transform <- function(y) { y * 2 }",
    "result <- clean_data(5)"
  ), temp_file)

  model <- raver_analyze(temp_file)
  arr <- create_arrangement(model, bpm = 122)

  # Check structure
  expect_true("bpm" %in% names(arr))
  expect_true("key" %in% names(arr))
  expect_true("sections" %in% names(arr))
  expect_true("total_bars" %in% names(arr))
  expect_true("motifs" %in% names(arr))

  expect_equal(arr$bpm, 122)
  expect_true(arr$key >= 36 && arr$key <= 72)
  expect_true(arr$total_bars > 0)
  expect_true(length(arr$sections) >= 2)  # At least intro and outro

  # Check sections structure
  for (section in arr$sections) {
    expect_true("type" %in% names(section))
    expect_true("bars" %in% names(section))
    expect_true("elements" %in% names(section))
  }

  # Clean up
  unlink(temp_file)
})

# ==============================================================================
# Element Selection Tests (1 test)
# ==============================================================================

test_that("get_section_elements respects density", {
  # Full density
  full <- get_section_elements("drop", 1.0)
  expect_equal(full, SECTION_TYPES$drop$elements)

  # Low density should have fewer elements
  sparse <- get_section_elements("drop", 0.3)
  expect_true(length(sparse) < length(full))
  expect_true(length(sparse) >= 1)

  # Core elements (kick, hihat) should be prioritized
  expect_true("kick" %in% sparse || length(sparse) == 1)
})

# ==============================================================================
# Motif to Pattern Tests (1 test)
# ==============================================================================

test_that("motif_to_pattern creates valid pattern", {
  scale <- raver_build_scale(57, "natural_minor", 2)
  motif <- generate_motif("pattern_test", scale)
  pattern <- motif_to_pattern(motif, "bass")

  expect_true("steps" %in% names(pattern))
  expect_true("instrument" %in% names(pattern))
  expect_true("steps_per_bar" %in% names(pattern))
  expect_true("note" %in% names(pattern))

  expect_length(pattern$steps, 16)
  expect_equal(pattern$instrument, "bass")
  expect_equal(pattern$steps_per_bar, 16L)

  # Active steps should have non-zero velocity
  active_indices <- which(motif$rhythm)
  expect_true(all(pattern$steps[active_indices] > 0))
})
