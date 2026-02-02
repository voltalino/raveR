#' @title User-Facing Composition API
#' @description High-level API functions for end-users. The main entry points
#'   are composeR() for file export and playR()/stopR() for live playback.
#' @name api
NULL

# Package-level state for active live controller
.live_env <- new.env(parent = emptyenv())

#' Compose R Script to Audio File
#'
#' Main user-facing function that transforms an R script into a deep house
#' track and exports it to an audio file. This is the primary entry point for
#' end-users.
#'
#' @param script_path Path to the R script file to transform into music
#' @param output_path Path for the output audio file. Format is detected from
#'   extension (.wav or .mp3)
#' @param bpm Numeric tempo (60-180 BPM). If NULL, auto-selects based on file hash within
#'   genre-specific range (default NULL)
#' @param seed Random seed for reproducibility. If NULL, uses file hash for
#'   determinism (default NULL)
#' @param genre Character: "deep_house", "techno", "ambient", "drum_bass", or "house"
#'   (default: "deep_house")
#'
#' @return The output_path invisibly on success
#' @export
#'
#' @details
#' The composeR() function is the primary user-facing API. It:
#' 1. Analyzes the R script to extract code structure
#' 2. Maps code features to musical parameters
#' 3. Generates a deep house track
#' 4. Exports to the specified format
#'
#' Supported output formats:
#' \itemize{
#'   \item WAV (.wav) - Uncompressed, highest quality
#'   \item MP3 (.mp3) - Compressed, requires FFmpeg
#' }
#'
#' Error handling:
#' \itemize{
#'   \item Non-existent script: Error with helpful message
#'   \item Invalid output format: Error listing supported formats
#'   \item Parse errors: Warning + graceful degradation to minimal composition
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - create WAV file
#' composeR("my_analysis.R", "output.wav")
#'
#' # Create MP3 file (requires FFmpeg)
#' composeR("my_analysis.R", "output.mp3")
#'
#' # With custom BPM
#' composeR("my_analysis.R", "output.wav", bpm = 122)
#'
#' # With explicit seed for reproducibility
#' composeR("my_analysis.R", "output.wav", seed = 12345)
#' }
composeR <- function(script_path, output_path, bpm = NULL, seed = NULL, genre = "deep_house") {
  # Validate script exists
  if (!file.exists(script_path)) {
    stop(
      "Script not found: '", script_path, "'",
      "\n  Please provide a valid path to an R script file.",
      call. = FALSE
    )
  }

  # Validate BPM range
  if (!is.null(bpm)) {
    bpm <- as.numeric(bpm)
    if (is.na(bpm) || bpm < 60 || bpm > 180) {
      stop(
        "BPM must be between 60 and 180 (got: ", bpm, ")",
        "\n  Extreme tempos can cause synthesis instability.",
        call. = FALSE
      )
    }
  }

  # Validate genre
  valid_genres <- c("deep_house", "techno", "ambient", "drum_bass")
  if (!genre %in% valid_genres) {
    stop(
      "Invalid genre: '", genre, "'",
      "\n  Valid genres: ", paste(valid_genres, collapse = ", "),
      call. = FALSE
    )
  }

  # Detect and validate output format from extension
  output_format <- detect_output_format(output_path)

  # Analyze the script (handles parse errors gracefully with warning)
  code_model <- raver_analyze(script_path)

  # Compose the track with genre
  track <- raver_compose(code_model, bpm = bpm, seed = seed, genre = genre)

  # Export based on format
  if (output_format == "wav") {
    export_wav(track, output_path)
  } else if (output_format == "mp3") {
    export_mp3(track, output_path)
  }

  invisible(output_path)
}

#' Detect Output Format from File Extension
#'
#' @description Internal helper that detects the audio format from a file path
#'   extension and validates it's a supported format.
#'
#' @param output_path Path to output file
#'
#' @return Character: "wav" or "mp3"
#' @keywords internal
detect_output_format <- function(output_path) {
  # Extract extension (case-insensitive)
  ext <- tolower(tools::file_ext(output_path))

  # Handle missing extension
  if (ext == "") {
    stop(
      "Cannot determine output format: no file extension in '", output_path, "'",
      "\n  Supported formats: .wav, .mp3",
      "\n  Example: composeR('script.R', 'output.wav')",
      call. = FALSE
    )
  }

  # Validate format
  supported_formats <- c("wav", "mp3")
  if (!(ext %in% supported_formats)) {
    stop(
      "Unsupported output format: '.", ext, "'",
      "\n  Supported formats: ", paste0(".", supported_formats, collapse = ", "),
      "\n  Example: composeR('script.R', 'output.wav')",
      call. = FALSE
    )
  }

  ext
}

#' Start Live Playback
#'
#' Starts continuous generative playback of deep house music derived from
#' an R script. The playback runs in the background while you continue to
#' work in R. If the script file is modified, the music will transition
#' smoothly to reflect the new code.
#'
#' @param script_path Path to the R script to sonify
#' @param bpm Numeric tempo (60-180). If NULL, auto-selects based on file and genre.
#' @param genre Character genre name. One of "deep_house", "techno", "ambient",
#'   "drum_bass", or "house". Default "deep_house".
#'
#' @return NULL invisibly. Use stopR() to stop playback.
#' @export
#'
#' @details
#' The playR() function starts a background playback loop that:
#' \itemize{
#'   \item Analyzes your R script to extract code structure
#'   \item Generates continuous deep house music based on the script
#'   \item Monitors the script file for changes
#'   \item Smoothly transitions the music when you modify and save the script
#' }
#'
#' The R console remains interactive during playback. Use stopR() to halt
#' playback when you're done.
#'
#' @examples
#' \dontrun{
#' # Start live playback
#' playR("my_analysis.R")
#'
#' # Edit and save my_analysis.R - music will transition
#' # ...work on your code...
#'
#' # Stop when done
#' stopR()
#'
#' # With custom BPM
#' playR("my_analysis.R", bpm = 122)
#' }
playR <- function(script_path, bpm = NULL, genre = "deep_house") {
  # Validate script exists
  if (!file.exists(script_path)) {
    stop("Script not found: ", script_path, call. = FALSE)
  }

  # Validate BPM range
  if (!is.null(bpm)) {
    bpm <- as.numeric(bpm)
    if (is.na(bpm) || bpm < 60 || bpm > 180) {
      stop("BPM must be between 60 and 180", call. = FALSE)
    }
  }

  # Validate genre
  valid_genres <- c("deep_house", "techno", "ambient", "drum_bass")
  if (!genre %in% valid_genres) {
    stop("Invalid genre. Valid: deep_house, techno, ambient, drum_bass", call. = FALSE)
  }

  # Stop any existing playback
  if (!is.null(.live_env$controller)) {
    stopR()
  }

  # Create controller with buffer limits for memory stability
  controller <- PlaybackController$new(script_path, bpm, genre, max_buffer_seconds = 300)

  # Store in package environment
  .live_env$controller <- controller

  # Start playback and file watching
  controller$start()

  message("Playing: ", basename(script_path))
  message("Modify the script and save to hear transitions.")
  message("Call stopR() to stop playback.")

  invisible(NULL)
}

#' Stop Live Playback
#'
#' Stops any active live playback started with playR().
#'
#' @return NULL invisibly.
#' @export
#'
#' @details
#' If no playback is currently active, this function does nothing but print
#' an informative message.
#'
#' @examples
#' \dontrun{
#' # After starting playback
#' playR("my_analysis.R")
#'
#' # Stop when done
#' stopR()
#' }
stopR <- function() {
  if (!is.null(.live_env$controller)) {
    .live_env$controller$stop()
    .live_env$controller <- NULL
    message("Playback stopped.")
  } else {
    message("No active playback.")
  }
  invisible(NULL)
}
