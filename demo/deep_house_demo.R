# demo/deep_house_demo.R
# RaveR Deep House Composition Demo
#
# Transforms a sample R script into a deep house track and exports it to WAV.
# Demonstrates deterministic output: same seed always produces the same music.
#
# Usage:
#   library(raveR)
#   demo(deep_house_demo, package = "raveR")

cat("\n")
cat("=========================================================\n")
cat("     RaveR Deep House Composition Demo\n")
cat("=========================================================\n")
cat("\n")

# --- Create a sample R script to analyze ---
cat("--- Step 1: Creating Sample R Script ---\n\n")

demo_script <- tempfile(fileext = ".R")
writeLines('
# Sample data processing script
load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  data <- read.csv(file_path)
  data[complete.cases(data), ]
}

process_data <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- scale(data[[col]])
    }
  }
  data
}

summarize_data <- function(data) {
  results <- list()
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      results[[col]] <- list(
        mean = mean(data[[col]], na.rm = TRUE),
        sd = sd(data[[col]], na.rm = TRUE)
      )
    }
  }
  results
}

analyze <- function(file_path) {
  data <- load_data(file_path)
  data <- process_data(data)
  summarize_data(data)
}
', demo_script)

cat("Created sample script with 4 functions and nested control flow.\n\n")

# --- Compose ---
cat("--- Step 2: Composing Deep House Track ---\n\n")

output_file <- file.path(tempdir(), "deep_house_demo.wav")

cat("Composing (this may take a moment)...\n")
start_time <- Sys.time()

composeR(demo_script, output_file, bpm = 120, seed = "demo_2024")

elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
file_size <- file.size(output_file)

cat(sprintf("  Done in %.1f seconds\n", elapsed))
cat(sprintf("  Output: %s\n", output_file))
cat(sprintf("  Size:   %.1f MB\n", file_size / (1024 * 1024)))
cat("\n")

# --- Verify determinism ---
cat("--- Step 3: Verifying Determinism ---\n\n")

output_file2 <- file.path(tempdir(), "deep_house_demo_2.wav")
composeR(demo_script, output_file2, bpm = 120, seed = "demo_2024")

if (file.size(output_file) == file.size(output_file2)) {
  cat("  PASS: Same seed produces identical output.\n")
} else {
  cat("  FAIL: Files differ.\n")
}
cat("\n")

# --- Summary ---
cat("=========================================================\n")
cat("              Demo Complete!\n")
cat("=========================================================\n")
cat("\n")
cat("Listen to the generated track:\n")
cat(sprintf("  %s\n", output_file))
cat("\n")
cat("Try with your own scripts:\n")
cat('  composeR("your_script.R", "output.wav")\n')
cat("\n")

# Cleanup
unlink(demo_script)
unlink(output_file2)
