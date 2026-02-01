# RaveR Live Demo
# ===============
#
# This demo shows live playback with file watching.
#
# Instructions:
# 1. Run this script line by line
# 2. When playR() starts, open the test script in your editor
# 3. Make changes to the script and save
# 4. Listen for the musical transition!
# 5. Try introducing a syntax error - hear the glitch
# 6. Fix the error - music transitions back to normal
# 7. Call stopR() when done
#
# Prerequisites:
# - raveR package installed or loaded via devtools::load_all()
# - Audio output working (speakers or headphones)

library(raveR)

# =============================================================================
# Step 1: Create a Simple Test Script
# =============================================================================

# Create a temporary test script that we'll edit during playback
test_script <- tempfile(fileext = ".R")
writeLines(c(
  "# Test script for RaveR live playback",
  "",
  "# Simple function - the music reflects code complexity",
  "my_function <- function(x) {",
  "  result <- x * 2",
  "  return(result)",
  "}"
), test_script)

cat("Test script created:", test_script, "\n")
cat("\n")
cat("IMPORTANT: Copy the path above and open it in your editor.\n")
cat("You'll modify this file while the music plays.\n")
cat("\n")

# =============================================================================
# Step 2: Start Live Playback
# =============================================================================

cat("Starting live playback...\n")
cat("The R console remains interactive - you can run other commands!\n")
cat("\n")

playR(test_script)

cat("\n")
cat("Music is now playing. Try these experiments:\n")
cat("\n")
cat("EXPERIMENT 1: Add complexity\n")
cat("  Open the test script and add more functions:\n")
cat("    another_func <- function(y) {\n")
cat("      if (y > 0) {\n")
cat("        return(y * 2)\n")
cat("      } else {\n")
cat("        return(0)\n")
cat("      }\n")
cat("    }\n")
cat("  Save the file and listen for the transition!\n")
cat("\n")
cat("EXPERIMENT 2: Trigger glitch\n")
cat("  Introduce a syntax error (delete a closing brace)\n")
cat("  Save and hear the glitch effect\n")
cat("\n")
cat("EXPERIMENT 3: Recover\n")
cat("  Fix the syntax error\n")
cat("  Save and hear the music transition back to clean\n")
cat("\n")
cat("When done, run: stopR()\n")
cat("\n")

# =============================================================================
# Step 3: Interactive Commands
# =============================================================================

# The console is interactive! You can:
# - Check status: message("Music is playing...")
# - Run other R code: 1 + 1
# - Stop playback: stopR()

# When you're done experimenting:
# stopR()

# Clean up the temporary script:
# unlink(test_script)
