# Tests for R script analysis pipeline
# Covers: parsing, error handling, metrics, mapping, CodeModel

# Helper to create temp R script file
create_test_script <- function(code) {
  file <- tempfile(fileext = ".R")
  writeLines(code, file)
  file
}

# =============================================================================
# Script Parsing Tests (raver_analyze)
# =============================================================================

test_that("raver_analyze extracts function definitions", {
  script <- create_test_script(c(
    "add <- function(x, y) x + y",
    "multiply <- function(a, b) a * b",
    "subtract <- function(x, y) x - y"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_s3_class(model, "CodeModel")
  expect_equal(length(model$functions), 3)
  expect_true("add" %in% model$functions)
  expect_true("multiply" %in% model$functions)
  expect_true("subtract" %in% model$functions)
})

test_that("raver_analyze extracts variable assignments", {
  script <- create_test_script(c(
    "x <- 10",
    "y = 20",
    "30 -> z"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_equal(length(model$variables), 3)
  expect_true("x" %in% model$variables)
  expect_true("y" %in% model$variables)
  expect_true("z" %in% model$variables)
})

test_that("raver_analyze extracts function calls", {
  script <- create_test_script(c(
    "result <- mean(c(1, 2, 3))",
    "output <- sum(result, na.rm = TRUE)",
    "print(output)"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_true("mean" %in% model$function_calls)
  expect_true("sum" %in% model$function_calls)
  expect_true("print" %in% model$function_calls)
  expect_true("c" %in% model$function_calls)
})

test_that("raver_analyze distinguishes functions from variables", {
  script <- create_test_script(c(
    "my_func <- function(x) x * 2",
    "my_var <- 42"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_true("my_func" %in% model$functions)
  expect_false("my_func" %in% model$variables)
  expect_true("my_var" %in% model$variables)
  expect_false("my_var" %in% model$functions)
})

# =============================================================================
# Error Handling Tests
# =============================================================================

test_that("raver_analyze handles syntax errors gracefully", {
  script <- create_test_script(c(
    "x <- function( {",
    "  broken syntax here"
  ))
  on.exit(unlink(script))

  # Should not error, just warn and return empty model
  expect_warning(
    model <- raver_analyze(script),
    "Parse error"
  )

  expect_s3_class(model, "CodeModel")
  # CodeModel ensures minimum viable values for music generation
  expect_equal(length(model$functions), 1)
  expect_equal(length(model$variables), 1)
  expect_equal(model$cyclomatic_complexity, 1L)
})

test_that("raver_analyze handles empty files", {
  script <- create_test_script("")
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_s3_class(model, "CodeModel")
  # CodeModel ensures minimum viable values for music generation
  expect_equal(length(model$functions), 1)
  expect_equal(length(model$variables), 1)
  expect_equal(model$nesting_depth, 0L)
})

test_that("raver_analyze handles comment-only files", {
  script <- create_test_script(c(
    "# This is a comment",
    "# Another comment",
    "## Section header"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_s3_class(model, "CodeModel")
  # CodeModel ensures minimum viable values for music generation
  expect_equal(length(model$functions), 1)
  expect_equal(length(model$variables), 1)
})

test_that("raver_analyze errors on missing file", {
  expect_error(
    raver_analyze("/nonexistent/path/to/file.R"),
    "File not found"
  )
})

# =============================================================================
# Complexity Metrics Tests
# =============================================================================

test_that("cyclomatic complexity increases with control flow", {
  simple <- create_test_script("x <- 1 + 2")
  complex <- create_test_script(c(
    "if (x > 0) { y <- 1 }",
    "for (i in 1:10) { z <- i }",
    "while (TRUE) { break }"
  ))
  on.exit(unlink(c(simple, complex)))

  simple_model <- raver_analyze(simple)
  complex_model <- raver_analyze(complex)

  expect_lt(simple_model$cyclomatic_complexity, complex_model$cyclomatic_complexity)
})

test_that("nesting depth reflects nested structures", {
  flat <- create_test_script(c(
    "if (x) { a <- 1 }",
    "if (y) { b <- 2 }"
  ))
  nested <- create_test_script(c(
    "if (x) {",
    "  if (y) {",
    "    if (z) {",
    "      result <- 1",
    "    }",
    "  }",
    "}"
  ))
  on.exit(unlink(c(flat, nested)))

  flat_model <- raver_analyze(flat)
  nested_model <- raver_analyze(nested)

  expect_lt(flat_model$nesting_depth, nested_model$nesting_depth)
  expect_equal(nested_model$nesting_depth, 3L)
})

test_that("control flow count tracks all control structures", {
  script <- create_test_script(c(
    "if (a) { x <- 1 }",
    "for (i in 1:10) {",
    "  while (i > 0) {",
    "    i <- i - 1",
    "  }",
    "}"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  # IF + FOR + WHILE = 3 control flow statements
  expect_gte(model$control_flow_count, 3L)
})

# =============================================================================
# Musical Mapping Tests
# =============================================================================

test_that("raver_map_to_music is deterministic", {
  script <- create_test_script(c(
    "clean <- function(x) x",
    "transform <- function(x) x * 2",
    "result <- clean(data)"
  ))
  on.exit(unlink(script))

  model1 <- raver_analyze(script)
  map1 <- raver_map_to_music(model1)

  model2 <- raver_analyze(script)
  map2 <- raver_map_to_music(model2)

  expect_identical(map1, map2)
})

test_that("function parts are in range 1-4", {
  script <- create_test_script(c(
    "a <- function(x) x",
    "b <- function(x) x",
    "c <- function(x) x",
    "d <- function(x) x",
    "e <- function(x) x",
    "f <- function(x) x"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)
  mapping <- raver_map_to_music(model)

  parts <- unlist(mapping$function_parts)
  expect_true(all(parts >= 1))
  expect_true(all(parts <= 4))
})

test_that("variable instruments are in range 1-5", {
  script <- create_test_script(c(
    "a <- 1",
    "b <- 2",
    "c <- 3",
    "d <- 4",
    "e <- 5",
    "f <- 6"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)
  mapping <- raver_map_to_music(model)

  instruments <- unlist(mapping$variable_instruments)
  expect_true(all(instruments >= 1))
  expect_true(all(instruments <= 5))
})

test_that("different scripts produce different mappings", {
  script1 <- create_test_script("foo <- function(x) x")
  script2 <- create_test_script("bar <- function(y) y")
  on.exit(unlink(c(script1, script2)))

  map1 <- raver_map_to_music(raver_analyze(script1))
  map2 <- raver_map_to_music(raver_analyze(script2))

  expect_false(identical(map1, map2))
})

test_that("raver_map_to_music handles empty code", {
  script <- create_test_script("# just a comment")
  on.exit(unlink(script))

  model <- raver_analyze(script)
  mapping <- raver_map_to_music(model)

  # CodeModel ensures minimum viable values, so mapping has 1 each
  expect_equal(length(mapping$function_parts), 1)
  expect_equal(length(mapping$variable_instruments), 1)
  expect_equal(mapping$arrangement$drum_intensity, 0)
})

test_that("drum_intensity increases with complexity", {
  simple <- create_test_script("x <- 1")
  complex <- create_test_script(c(
    "for (i in 1:10) {",
    "  for (j in 1:i) {",
    "    if (i > j) {",
    "      while (TRUE) {",
    "        break",
    "      }",
    "    }",
    "  }",
    "}"
  ))
  on.exit(unlink(c(simple, complex)))

  simple_map <- raver_map_to_music(raver_analyze(simple))
  complex_map <- raver_map_to_music(raver_analyze(complex))

  expect_lt(simple_map$arrangement$drum_intensity,
            complex_map$arrangement$drum_intensity)
})

test_that("raver_map_to_music validates input", {
  expect_error(
    raver_map_to_music(list(not = "a CodeModel")),
    "must be a CodeModel"
  )
})

# =============================================================================
# CodeModel Tests
# =============================================================================

test_that("CodeModel is_same_as detects identical files", {
  script <- create_test_script("x <- 1")
  on.exit(unlink(script))

  model1 <- raver_analyze(script)
  model2 <- raver_analyze(script)

  expect_true(model1$is_same_as(model2))
  expect_true(model2$is_same_as(model1))
})

test_that("CodeModel is_same_as detects changed files", {
  script <- create_test_script("x <- 1")
  model1 <- raver_analyze(script)

  # Modify the file
  writeLines("x <- 2", script)
  on.exit(unlink(script))

  model2 <- raver_analyze(script)

  expect_false(model1$is_same_as(model2))
})

test_that("part_count scales with function count", {
  few_funcs <- create_test_script(c(
    "a <- function(x) x"
  ))
  many_funcs <- create_test_script(c(
    "a <- function(x) x",
    "b <- function(x) x",
    "c <- function(x) x",
    "d <- function(x) x",
    "e <- function(x) x",
    "f <- function(x) x",
    "g <- function(x) x",
    "h <- function(x) x"
  ))
  on.exit(unlink(c(few_funcs, many_funcs)))

  few_model <- raver_analyze(few_funcs)
  many_model <- raver_analyze(many_funcs)

  # More functions should generally produce more parts (up to max of 4)
  expect_lte(few_model$part_count, many_model$part_count)
  expect_lte(many_model$part_count, 4L)
})

test_that("instrument_count scales with variable count", {
  few_vars <- create_test_script("x <- 1")
  many_vars <- create_test_script(c(
    "a <- 1",
    "b <- 2",
    "c <- 3",
    "d <- 4",
    "e <- 5",
    "f <- 6"
  ))
  on.exit(unlink(c(few_vars, many_vars)))

  few_model <- raver_analyze(few_vars)
  many_model <- raver_analyze(many_vars)

  expect_lte(few_model$instrument_count, many_model$instrument_count)
  expect_lte(many_model$instrument_count, 5L)
})

test_that("CodeModel summary returns string with key info", {
  script <- create_test_script(c(
    "my_func <- function(x) x",
    "my_var <- 42"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)
  summary_str <- model$summary()

  expect_type(summary_str, "character")
  expect_true(grepl("1 functions", summary_str))
  expect_true(grepl("1 variables", summary_str))
})

test_that("CodeModel stores file path and hash", {
  script <- create_test_script("x <- 1")
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_equal(model$file_path, script)
  expect_type(model$file_hash, "character")
  expect_equal(nchar(model$file_hash), 32)  # MD5 hash is 32 hex chars
})

test_that("CodeModel stores line count", {
  script <- create_test_script(c(
    "line1 <- 1",
    "line2 <- 2",
    "line3 <- 3"
  ))
  on.exit(unlink(script))

  model <- raver_analyze(script)

  expect_equal(model$line_count, 3L)
})
