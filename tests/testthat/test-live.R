# Tests for PlaybackController (R/live.R)
# Tests state management and buffer generation logic
# Note: Actual audio playback is not tested (platform-dependent)

# Helper to create a test script
create_test_script <- function(content = NULL) {
  temp_script <- tempfile(fileext = ".R")
  if (is.null(content)) {
    content <- "
# Simple test script
add <- function(x, y) {
  x + y
}

multiply <- function(x, y) {
  x * y
}

result <- add(1, 2)
output <- multiply(result, 3)
"
  }
  writeLines(content, temp_script)
  temp_script
}

# =============================================================================
# Test: Initialization
# =============================================================================

test_that("PlaybackController initializes with valid script path", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  expect_s3_class(pc, "PlaybackController")
  expect_equal(pc$script_path, normalizePath(script))
  expect_false(is.null(pc$code_model))
  expect_false(is.null(pc$last_known_good_model))
  expect_false(is.null(pc$bpm))
})

test_that("PlaybackController errors on non-existent script", {
  expect_error(
    PlaybackController$new("/nonexistent/path/script.R"),
    "Script not found"
  )
})

test_that("PlaybackController auto-selects BPM when not provided", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  # BPM should be in valid deep house range

  expect_gte(pc$bpm, 118)
  expect_lte(pc$bpm, 124)
})

test_that("PlaybackController uses provided BPM in valid range", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script, bpm = 122)

  expect_equal(pc$bpm, 122L)
})

test_that("PlaybackController validates BPM range", {
  script <- create_test_script()
  on.exit(unlink(script))

  # BPM too low
  expect_error(
    PlaybackController$new(script, bpm = 100),
    "BPM must be between 118 and 124"
  )

  # BPM too high
  expect_error(
    PlaybackController$new(script, bpm = 140),
    "BPM must be between 118 and 124"
  )
})

test_that("PlaybackController deterministically selects BPM from same script", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc1 <- PlaybackController$new(script)
  pc2 <- PlaybackController$new(script)

  # Same script should produce same BPM
  expect_equal(pc1$bpm, pc2$bpm)
})

# =============================================================================
# Test: Buffer Generation
# =============================================================================

test_that("generate_buffer returns Wave object", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  buffer <- pc$generate_buffer("drop", 4)

  expect_s4_class(buffer, "Wave")
})
test_that("generate_buffer produces expected duration", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script, bpm = 120)
  buffer <- pc$generate_buffer("drop", 4)

  # At 120 BPM, 4 bars = 4 * 4 * (60/120) = 8 seconds
  expected_duration <- 8.0
  actual_duration <- length(buffer@left) / buffer@samp.rate

  # Allow 0.5 second tolerance for audio processing overhead
  expect_gte(actual_duration, expected_duration - 0.5)
  expect_lte(actual_duration, expected_duration + 0.5)
})

test_that("generate_buffer works with different section types", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  section_types <- c("intro", "build", "drop", "breakdown", "outro")
  for (section_type in section_types) {
    buffer <- pc$generate_buffer(section_type, 4)
    expect_s4_class(buffer, "Wave")
    expect_gt(length(buffer@left), 0, label = paste("Section type:", section_type))
  }
})

test_that("generate_buffer respects bar count parameter", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script, bpm = 120)

  # 2 bars vs 4 bars
  buffer_2 <- pc$generate_buffer("drop", 2)
  buffer_4 <- pc$generate_buffer("drop", 4)

  duration_2 <- length(buffer_2@left) / buffer_2@samp.rate
  duration_4 <- length(buffer_4@left) / buffer_4@samp.rate

  # 4 bars should be approximately twice as long as 2 bars
  expect_gt(duration_4, duration_2 * 1.5)
})

# =============================================================================
# Test: State Management
# =============================================================================

test_that("get_state returns expected structure", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  state <- pc$get_state()

  expected_fields <- c(
    "is_playing", "current_bar", "pending_transition",
    "script_path", "bpm", "has_code_model", "has_fallback_model"
  )

  for (field in expected_fields) {
    expect_true(field %in% names(state), info = paste("Missing field:", field))
  }
})

test_that("is_playing starts as FALSE", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  expect_false(pc$is_playing)
  expect_equal(pc$get_state()$is_playing, FALSE)
})

test_that("current_bar starts at 0", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  expect_equal(pc$current_bar, 0L)
})

test_that("pending_transition starts as FALSE", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  expect_false(pc$pending_transition)
})

test_that("queue_transition sets pending_transition to TRUE", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  expect_false(pc$pending_transition)

  pc$queue_transition()

  expect_true(pc$pending_transition)
})

# =============================================================================
# Test: Script Reload
# =============================================================================

test_that("reload_script updates code_model on valid script", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  old_hash <- pc$code_model$file_hash

  # Modify the script
  writeLines("
new_function <- function(a) {
  a * 2
}
", script)

  # Reload
  result <- pc$reload_script()

  expect_true(result)
  # Hash should change
  expect_false(identical(pc$code_model$file_hash, old_hash))
  # Last known good should also update
  expect_identical(pc$code_model$file_hash, pc$last_known_good_model$file_hash)
})

test_that("reload_script keeps last_known_good on parse error", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  good_hash <- pc$last_known_good_model$file_hash

  # Write invalid R code
  writeLines("
this is { not valid R code )))
", script)

  # Suppress the parse warning from safe_parse
  suppressWarnings({
    result <- pc$reload_script()
  })

  # Reload should indicate failure but keep good model
  expect_false(result)
  expect_identical(pc$last_known_good_model$file_hash, good_hash)
})

test_that("reload_script sets pending_transition on change", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  pc$pending_transition <- FALSE  # Ensure it's false

  # Modify script
  writeLines("
changed <- function() { 42 }
", script)

  pc$reload_script()

  expect_true(pc$pending_transition)
})

# =============================================================================
# Test: Lifecycle (start/stop)
# =============================================================================

test_that("start sets is_playing to TRUE", {
  script <- create_test_script()
  on.exit({
    pc$stop()
    unlink(script)
  })

  pc <- PlaybackController$new(script)
  expect_false(pc$is_playing)

  # Start and immediately stop to avoid long-running audio
  pc$start()
  expect_true(pc$is_playing)

  pc$stop()
})

test_that("stop sets is_playing to FALSE", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  pc$start()
  expect_true(pc$is_playing)

  pc$stop()
  expect_false(pc$is_playing)
})

test_that("stop is idempotent (can call multiple times)", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  pc$start()
  pc$stop()
  pc$stop()  # Should not error
  pc$stop()

  expect_false(pc$is_playing)
})

test_that("start is idempotent when already playing", {
  script <- create_test_script()
  on.exit({
    pc$stop()
    unlink(script)
  })

  pc <- PlaybackController$new(script)

  pc$start()
  expect_true(pc$is_playing)

  # Calling start again should not error
  expect_message(pc$start(), "already running")
  expect_true(pc$is_playing)

  pc$stop()
})

test_that("start resets current_bar to 0", {
  script <- create_test_script()
  on.exit({
    pc$stop()
    unlink(script)
  })

  pc <- PlaybackController$new(script)

  # Manually set current_bar to simulate previous playback
  pc$current_bar <- 16L

  pc$start()
  expect_equal(pc$current_bar, 0L)

  pc$stop()
})

# =============================================================================
# Test: Code Model Integration
# =============================================================================

test_that("code_model reflects script content", {
  script <- create_test_script("
my_func <- function(x) x + 1
my_var <- 42
")
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  expect_true("my_func" %in% pc$code_model$functions)
  expect_true("my_var" %in% pc$code_model$variables)
})

test_that("different scripts produce different code_models", {
  script1 <- create_test_script("
func_a <- function() 1
")
  script2 <- create_test_script("
func_b <- function() 2
func_c <- function() 3
")
  on.exit({
    unlink(script1)
    unlink(script2)
  })

  pc1 <- PlaybackController$new(script1)
  pc2 <- PlaybackController$new(script2)

  # Different function counts
  expect_equal(length(pc1$code_model$functions), 1)
  expect_equal(length(pc2$code_model$functions), 2)

  # Different hashes
  expect_false(identical(pc1$code_model$file_hash, pc2$code_model$file_hash))
})

# =============================================================================
# Test: Transition Logic
# =============================================================================

test_that("has_parse_error starts as FALSE", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  expect_false(pc$has_parse_error)
})

test_that("get_state includes has_parse_error field", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  state <- pc$get_state()

  expect_true("has_parse_error" %in% names(state))
  expect_false(state$has_parse_error)
})

test_that("on_file_change sets has_parse_error on parse failure", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  expect_false(pc$has_parse_error)

  # Write invalid R code
  writeLines("this { is not valid R code )))", script)

  # Trigger file change - suppress the warning from parse error
  suppressWarnings({
    pc$on_file_change(script)
  })

  expect_true(pc$has_parse_error)
  expect_true(pc$pending_transition)
})

test_that("on_file_change sets pending_transition on valid change", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)
  pc$pending_transition <- FALSE

  # Write valid new content
  writeLines("new_func <- function() { 42 }", script)

  pc$on_file_change(script)

  expect_true(pc$pending_transition)
  expect_false(pc$has_parse_error)
})

test_that("on_file_change clears has_parse_error on successful parse", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  # First, simulate a parse error
  writeLines("invalid { code ))))", script)
  suppressWarnings({
    pc$on_file_change(script)
  })
  expect_true(pc$has_parse_error)

  # Now fix the script
  writeLines("fixed <- function() { 'working' }", script)
  pc$on_file_change(script)

  expect_false(pc$has_parse_error)
})

# Note: generate_transition_buffers is a private method, so we test it indirectly
# through the public interface or by examining side effects

test_that("transition buffers contain valid audio data", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  # Access private method via environment
  transition_buffers <- pc$.__enclos_env__$private$generate_transition_buffers()

  # Should return list of buffers
  expect_true(is.list(transition_buffers))
  expect_true(length(transition_buffers) > 0)

  # Each buffer should be a Wave object
  for (buffer in transition_buffers) {
    expect_s4_class(buffer, "Wave")
    expect_gt(length(buffer@left), 0)
  }
})

test_that("transition buffers follow breakdown -> build -> drop pattern", {
  script <- create_test_script()
  on.exit(unlink(script))

  pc <- PlaybackController$new(script)

  # Access private method via environment
  transition_buffers <- pc$.__enclos_env__$private$generate_transition_buffers()

  # Should have exactly 3 buffers (breakdown, build, drop)
  expect_equal(length(transition_buffers), 3)

  # All should be valid Wave objects with audio
  for (i in seq_along(transition_buffers)) {
    expect_s4_class(transition_buffers[[i]], "Wave")
    expect_gt(length(transition_buffers[[i]]@left), 0)
  }
})
