#' @title Cross-Platform Audio Playback
#' @description Functions for playing audio through system audio drivers.
#'   Uses the audio package for native playback on Mac (AudioUnits),
#'   Windows (Windows Multimedia), and Linux (PortAudio).
#' @name audio-playback
NULL

# Package environment to store current playback state
# This enables stop_audio() to work without passing instance explicitly
.playback_env <- new.env(parent = emptyenv())

#' Play Audio
#'
#' Plays a Wave object through the system audio drivers. Playback is
#' asynchronous - the function returns immediately while audio plays
#' in the background. Use wait_audio() to block until playback completes.
#'
#' @param wave A tuneR Wave object to play
#'
#' @return An audioInstance object for playback control (invisibly)
#' @export
#'
#' @details
#' Platform notes:
#' \itemize{
#'   \item macOS: AudioUnits - works silently in background
#'   \item Windows: Windows Multimedia - may open external player window
#'   \item Linux: PortAudio - experimental, may require setup
#' }
#'
#' The Wave object is normalized to [-1, 1] range for the audio package.
#' For 32-bit float input, this involves scaling by the max absolute value.
#' For 16-bit PCM input, this scales by 32768.
#'
#' @examples
#' \dontrun{
#' # Play a 440 Hz sine wave
#' tone <- raver_sine(440, 1)
#' instance <- play_audio(tone)
#'
#' # Wait for playback to complete
#' wait_audio(instance)
#'
#' # Or play and stop early
#' tone <- raver_sine(440, 5)
#' play_audio(tone)
#' Sys.sleep(1)
#' stop_audio()
#' }
play_audio <- function(wave) {
  # Validate input

if (!inherits(wave, "Wave")) {
    stop("wave must be a tuneR Wave object")
  }

  # Platform warnings
  os_type <- .Platform$OS.type
  sys_name <- Sys.info()["sysname"]

  if (sys_name == "Windows") {
    message("Note: Windows audio playback may open an external player window. ",
            "Use export_wav() as an alternative.")
  } else if (sys_name == "Linux") {
    message("Note: Linux audio uses PortAudio (experimental). ",
            "If playback fails, use export_wav() to create a file.")
  }

  # Normalize samples to [-1, 1] range for audio package
  samples <- wave@left

  if (wave@bit == 32) {
    # 32-bit float: scale by max absolute value to avoid clipping
    max_val <- max(abs(samples))
    if (max_val > 0) {
      normalized <- samples / max_val
    } else {
      normalized <- samples
    }
  } else if (wave@bit == 16) {
    # 16-bit PCM: scale by 32768
    normalized <- samples / 32768
  } else {
    # Other bit depths: try generic normalization
    max_val <- max(abs(samples))
    if (max_val > 0) {
      normalized <- samples / max_val
    } else {
      normalized <- samples
    }
  }

  # Create audio matrix (mono or stereo)
  if (wave@stereo) {
    # Stereo: interleave left and right channels
    right <- wave@right
    if (wave@bit == 32) {
      max_val_r <- max(abs(right))
      if (max_val_r > 0) {
        normalized_r <- right / max(abs(wave@left), max_val_r)
      } else {
        normalized_r <- right
      }
    } else if (wave@bit == 16) {
      normalized_r <- right / 32768
    } else {
      max_val_r <- max(abs(right))
      if (max_val_r > 0) {
        normalized_r <- right / max_val_r
      } else {
        normalized_r <- right
      }
    }
    audio_data <- rbind(normalized, normalized_r)
  } else {
    # Mono
    audio_data <- normalized
  }

  # Play audio asynchronously
  instance <- tryCatch({
    audio::play(audio_data, rate = wave@samp.rate)
  }, error = function(e) {
    # Provide helpful error message
    stop(sprintf(
      "Audio playback failed: %s\n\nTroubleshooting:\n%s",
      e$message,
      paste(
        "- macOS: Should work automatically with AudioUnits",
        "- Windows: Ensure audio output device is available",
        "- Linux: PortAudio may need to be installed: sudo apt-get install portaudio19-dev",
        "- Alternative: Use export_wav() to save audio to a file and play with external player",
        sep = "\n"
      )
    ))
  })

  # Store instance for stop_audio() and wait_audio()
  .playback_env$current_instance <- instance

  invisible(instance)
}

#' Stop Audio Playback
#'
#' Stops the current audio playback. Safe to call even when nothing
#' is playing - the function will simply do nothing.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Play a long tone and stop it early
#' tone <- raver_sine(440, 10)
#' play_audio(tone)
#' Sys.sleep(2)
#' stop_audio()  # Stops after ~2 seconds
#' }
stop_audio <- function() {
  instance <- .playback_env$current_instance

  if (!is.null(instance)) {
    tryCatch({
      audio::close(instance)
    }, error = function(e) {
      # Silently ignore errors (e.g., if already stopped)
    })
    .playback_env$current_instance <- NULL
  }

  invisible(NULL)
}

#' Wait for Audio Playback
#'
#' Blocks execution until the audio playback completes. If no instance
#' is provided, waits for the most recently started playback.
#'
#' @param instance An audioInstance object from play_audio(). If NULL,
#'   uses the most recently started playback.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' # Play and wait for completion
#' tone <- raver_sine(440, 2)
#' play_audio(tone)
#' wait_audio()  # Blocks for ~2 seconds
#'
#' # Or with explicit instance
#' instance <- play_audio(tone)
#' wait_audio(instance)
#' }
wait_audio <- function(instance = NULL) {
  if (is.null(instance)) {
    instance <- .playback_env$current_instance
  }

  if (!is.null(instance)) {
    tryCatch({
      audio::wait(instance)
    }, error = function(e) {
      # Silently ignore errors (e.g., if already complete)
    })
  }

  invisible(NULL)
}

#' Pause Audio Playback
#'
#' Pauses the current audio playback. Can be resumed with resume_audio().
#'
#' @param instance An audioInstance object from play_audio(). If NULL,
#'   uses the most recently started playback.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' tone <- raver_sine(440, 10)
#' play_audio(tone)
#' Sys.sleep(2)
#' pause_audio()   # Pauses at ~2 seconds
#' Sys.sleep(1)
#' resume_audio()  # Resumes playback
#' }
pause_audio <- function(instance = NULL) {
  if (is.null(instance)) {
    instance <- .playback_env$current_instance
  }

  if (!is.null(instance)) {
    tryCatch({
      audio::pause(instance)
    }, error = function(e) {
      # Silently ignore errors
    })
  }

  invisible(NULL)
}

#' Resume Audio Playback
#'
#' Resumes paused audio playback.
#'
#' @param instance An audioInstance object from play_audio(). If NULL,
#'   uses the most recently started playback.
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' tone <- raver_sine(440, 10)
#' play_audio(tone)
#' Sys.sleep(2)
#' pause_audio()
#' Sys.sleep(1)
#' resume_audio()  # Continues from where it paused
#' }
resume_audio <- function(instance = NULL) {
  if (is.null(instance)) {
    instance <- .playback_env$current_instance
  }

  if (!is.null(instance)) {
    tryCatch({
      audio::resume(instance)
    }, error = function(e) {
      # Silently ignore errors
    })
  }

  invisible(NULL)
}
