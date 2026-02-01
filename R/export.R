#' @title Audio File Export
#' @description Functions for exporting Wave objects to audio files (WAV, MP3).
#'   WAV export uses tuneR::writeWave() with 16-bit PCM format for maximum
#'   compatibility. MP3 export uses av package (requires FFmpeg).
#' @name audio-export
NULL

#' Export to WAV File
#'
#' Exports a Wave object to a WAV file. The audio is normalized to 16-bit
#' PCM format (CD quality) for maximum compatibility with audio players.
#'
#' @param wave A tuneR Wave object to export
#' @param filename Output filename. The .wav extension is added if missing.
#' @param bit_depth Bit depth for export (default: 16 for CD quality)
#'
#' @return The full path to the created file (invisibly)
#' @export
#'
#' @details
#' The Wave object is normalized using normalize_for_export() before writing.
#' This ensures proper amplitude scaling and bit depth conversion from the
#' 32-bit float internal format to 16-bit PCM.
#'
#' The file is written with extensible=FALSE for maximum compatibility with
#' audio players and software.
#'
#' @examples
#' \dontrun{
#' # Generate and export a sine wave
#' tone <- raver_sine(440, 2)
#' path <- export_wav(tone, "my_tone.wav")
#' print(path)  # Full path to file
#'
#' # Extension added automatically
#' path <- export_wav(tone, "my_tone")  # Creates my_tone.wav
#' }
export_wav <- function(wave, filename, bit_depth = 16L) {
  # Validate input
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object")
  }

  # Validate sample rate
  if (wave@samp.rate != SAMPLE_RATE) {
    warning(sprintf(
      "Wave sample rate (%d Hz) differs from standard (%d Hz). ",
      wave@samp.rate, SAMPLE_RATE
    ))
  }

  # Add .wav extension if missing
  if (!grepl("\\.[Ww][Aa][Vv]$", filename)) {
    filename <- paste0(filename, ".wav")
  }

  # Convert to absolute path
  if (!startsWith(filename, "/") && !grepl("^[A-Za-z]:", filename)) {
    filename <- file.path(getwd(), filename)
  }

  # Validate parent directory exists
  parent_dir <- dirname(filename)
  if (!dir.exists(parent_dir)) {
    stop(sprintf(
      "Cannot write to '%s': directory '%s' does not exist",
      filename, parent_dir
    ))
  }

  # Normalize to export bit depth
  if (bit_depth == 16L) {
    export_wave <- normalize_for_export(wave)
  } else if (bit_depth == 32L) {
    # Keep as-is for 32-bit export
    export_wave <- wave
  } else {
    stop(sprintf("Unsupported bit depth: %d. Use 16 or 32.", bit_depth))
  }

  # Write WAV file
  tryCatch({
    tuneR::writeWave(export_wave, filename = filename, extensible = FALSE)
  }, error = function(e) {
    stop(sprintf(
      "Failed to write WAV file '%s': %s",
      filename, e$message
    ))
  })

  message(sprintf("Exported WAV: %s", filename))
  invisible(filename)
}

#' Check FFmpeg Availability
#'
#' Checks if FFmpeg is available for MP3 export. Returns TRUE if MP3
#' export will work, FALSE with a warning message if not.
#'
#' @return Logical: TRUE if FFmpeg is available for MP3 export, FALSE otherwise
#' @export
#'
#' @details
#' MP3 export requires FFmpeg to be installed on the system with the lame
#' codec (MP3 encoder). This function checks for FFmpeg availability via
#' the av package.
#'
#' Installation instructions:
#' \itemize{
#'   \item macOS: \code{brew install ffmpeg}
#'   \item Ubuntu/Debian: \code{sudo apt-get install ffmpeg}
#'   \item Windows: Download from https://ffmpeg.org/download.html and add to PATH
#' }
#'
#' @examples
#' \dontrun{
#' if (check_ffmpeg()) {
#'   export_mp3(my_audio, "output.mp3")
#' } else {
#'   export_wav(my_audio, "output.wav")  # Fallback
#' }
#' }
check_ffmpeg <- function() {
  # Try to get available muxers from av package
  available <- tryCatch({
    muxers <- av::av_muxers()
    # Check if MP3 muxer is available
    "mp3" %in% muxers$name
  }, error = function(e) {
    FALSE
  })

  if (!available) {
    warning(paste(
      "FFmpeg with MP3 support not available.",
      "MP3 export will not work.",
      "",
      "Installation instructions:",
      "  macOS: brew install ffmpeg",
      "  Ubuntu/Debian: sudo apt-get install ffmpeg",
      "  Windows: Download from https://ffmpeg.org/download.html",
      "",
      "Use export_wav() as an alternative.",
      sep = "\n"
    ))
  }

  available
}

#' Export to MP3 File
#'
#' Exports a Wave object to an MP3 file. Requires FFmpeg to be installed
#' on the system. The audio is first exported to a temporary WAV file,
#' then converted to MP3 using av::av_audio_convert().
#'
#' @param wave A tuneR Wave object to export
#' @param filename Output filename. The .mp3 extension is added if missing.
#' @param bitrate Bitrate in bits per second (default: 192000 for 192 kbps)
#'
#' @return The full path to the created file (invisibly)
#' @export
#'
#' @details
#' MP3 export requires FFmpeg with the lame codec. Use check_ffmpeg() to
#' verify availability before calling this function.
#'
#' The default bitrate of 192 kbps provides good quality for most uses.
#' Higher bitrates (256000, 320000) provide better quality at larger file size.
#' Lower bitrates (128000, 96000) provide smaller files with reduced quality.
#'
#' @examples
#' \dontrun{
#' # Check FFmpeg first
#' if (check_ffmpeg()) {
#'   tone <- raver_sine(440, 2)
#'   path <- export_mp3(tone, "my_tone.mp3")
#'
#'   # With custom bitrate (320 kbps)
#'   path <- export_mp3(tone, "high_quality.mp3", bitrate = 320000L)
#' }
#' }
export_mp3 <- function(wave, filename, bitrate = 192000L) {
  # Validate input
  if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object")
  }

  # Check FFmpeg availability
  if (!check_ffmpeg()) {
    stop(paste(
      "Cannot export MP3: FFmpeg not available.",
      "",
      "Installation instructions:",
      "  macOS: brew install ffmpeg",
      "  Ubuntu/Debian: sudo apt-get install ffmpeg",
      "  Windows: Download from https://ffmpeg.org/download.html",
      "",
      "Alternative: Use export_wav() to create a WAV file instead.",
      sep = "\n"
    ))
  }

  # Add .mp3 extension if missing
  if (!grepl("\\.[Mm][Pp]3$", filename)) {
    filename <- paste0(filename, ".mp3")
  }

  # Convert to absolute path
  if (!startsWith(filename, "/") && !grepl("^[A-Za-z]:", filename)) {
    filename <- file.path(getwd(), filename)
  }

  # Validate parent directory exists
  parent_dir <- dirname(filename)
  if (!dir.exists(parent_dir)) {
    stop(sprintf(
      "Cannot write to '%s': directory '%s' does not exist",
      filename, parent_dir
    ))
  }

  # Create temporary WAV file
  temp_wav <- tempfile(fileext = ".wav")
  on.exit(unlink(temp_wav), add = TRUE)

  # Export to temporary WAV
  export_wave <- normalize_for_export(wave)
  tuneR::writeWave(export_wave, filename = temp_wav, extensible = FALSE)

  # Convert to MP3
  tryCatch({
    # Determine number of channels
    channels <- if (wave@stereo) 2L else 1L

    av::av_audio_convert(
      audio = temp_wav,
      output = filename,
      bit_rate = bitrate,
      sample_rate = wave@samp.rate,
      channels = channels
    )
  }, error = function(e) {
    stop(sprintf(
      "Failed to create MP3 file '%s': %s\n\n%s",
      filename, e$message,
      "If FFmpeg is installed but this fails, try export_wav() instead."
    ))
  })

  message(sprintf("Exported MP3: %s (bitrate: %d kbps)", filename, bitrate / 1000))
  invisible(filename)
}
