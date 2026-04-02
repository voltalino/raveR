# demo/live_demo.R
# RaveR Live Playback Demo
#
# Demonstrates live coding mode: playR() starts background playback that
# responds to code changes in real-time. Requires the 'watcher' package.
#
# Instructions:
# 1. Run this script (or step through it line by line)
# 2. When playR() starts, open the test script in your editor
# 3. Make changes and save -- listen for the musical transition
# 4. Call stopR() when done
#
# Usage:
#   library(raveR)
#   demo(live_demo, package = "raveR")

library(raveR)

# --- Create a test script ---
test_script <- tempfile(fileext = ".R")
writeLines(c(
  "# Test script for raveR live playback",
  "",
  "my_function <- function(x) {",
  "  result <- x * 2",
  "  return(result)",
  "}"
), test_script)

cat("\n")
cat("Test script created:\n")
cat(sprintf("  %s\n", test_script))
cat("\n")
cat("Open this file in your editor. You will modify it while the music plays.\n")
cat("\n")

# --- Start live playback ---
playR(test_script)

cat("\n")
cat("Experiments to try:\n")
cat("\n")
cat("1. ADD COMPLEXITY: paste more functions and save the file.\n")
cat("   More functions = more musical parts.\n")
cat("\n")
cat("2. TRIGGER GLITCH: introduce a syntax error (delete a brace).\n")
cat("   Save and hear the glitch effect.\n")
cat("\n")
cat("3. RECOVER: fix the error and save. Music transitions back.\n")
cat("\n")
cat("When done, run: stopR()\n")
cat("Clean up:       unlink('", test_script, "')\n", sep = "")
cat("\n")
