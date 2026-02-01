#' @title Live Playback Controller
#' @description R6 class for managing continuous audio playback with pre-rendered
#'   buffers. Provides the core playback loop infrastructure for live mode and
#'   file watching functionality.
#' @name live
NULL

#' PlaybackController Class
#'
#' An R6 class that manages continuous audio playback with pre-rendered buffers.
#' Uses later::later() for non-blocking scheduling and tracks playback state
#' including bar position and pending transitions.
#'
#' @details
#' The controller generates 4-bar audio buffers (approximately 8 seconds at 120 BPM),
#' which provides enough lookahead time to generate the next buffer while the
#' current one plays. This approach handles R's GC pauses gracefully.
#'
#' Key features:
#' \itemize{
#'   \item Non-blocking playback with later::later() scheduling
#'   \item 4-bar buffer generation using raver_compose_section()
#'   \item State tracking for bar position and transitions
#'   \item Graceful error handling with last_known_good_model fallback
#'   \item Pending transition flag for smooth phrase-boundary changes
#' }
#'
#' @export
PlaybackController <- R6::R6Class(
  "PlaybackController",

  public = list(
    #' @field script_path Path to the R script being played
    script_path = NULL,

    #' @field code_model Current CodeModel from script analysis
    code_model = NULL,

    #' @field bpm Beats per minute (118-124 range for deep house)
    bpm = NULL,

    #' @field is_playing Whether playback is currently active
    is_playing = FALSE,

    #' @field current_bar Current bar position in playback (0-indexed)
    current_bar = 0L,

    #' @field pending_transition Whether a transition is queued for next phrase
    pending_transition = FALSE,

    #' @field last_known_good_model Last successfully parsed CodeModel for fallback
    last_known_good_model = NULL,

    #' @field has_parse_error Whether the current script has a parse error
    has_parse_error = FALSE,

    #' Initialize PlaybackController
    #'
    #' @param script_path Path to the R script to play
    #' @param bpm Optional BPM (118-124). If NULL, auto-selects based on script hash.
    #'
    #' @return A new PlaybackController instance
    initialize = function(script_path, bpm = NULL) {
      # Validate script exists
      if (!file.exists(script_path)) {
        stop("Script not found: ", script_path, call. = FALSE)
      }

      # Store script path
      self$script_path <- normalizePath(script_path)

      # Analyze script to create code model
      self$code_model <- raver_analyze(self$script_path)
      self$last_known_good_model <- self$code_model

      # Auto-select or validate BPM
      if (is.null(bpm)) {
        self$bpm <- select_bpm(self$code_model)
      } else {
        validate_bpm(bpm)
        self$bpm <- as.integer(bpm)
      }

      invisible(self)
    },

    #' Generate Audio Buffer
    #'
    #' Generates a 4-bar audio buffer using raver_compose_section().
    #' Returns a Wave object ready for playback.
    #'
    #' @param section_type Section type: "intro", "build", "drop", "breakdown", "outro"
    #' @param bars Number of bars to generate. Default 4.
    #'
    #' @return A tuneR Wave object containing the generated audio
    generate_buffer = function(section_type = "drop", bars = 4L) {
      # Use current code model (or last known good on error)
      model <- if (!is.null(self$code_model)) {
        self$code_model
      } else {
        self$last_known_good_model
      }

      if (is.null(model)) {
        stop("No code model available for buffer generation", call. = FALSE)
      }

      # Generate section using raver_compose_section
      raver_compose_section(
        code_model = model,
        section_type = section_type,
        bpm = self$bpm,
        bars = as.integer(bars)
      )
    },

    #' Start Playback
    #'
    #' Begins continuous playback. Generates a long buffer and loops it.
    #' Only regenerates when file changes are detected.
    #'
    #' @return NULL (invisibly)
    start = function() {
      if (self$is_playing) {
        message("Playback already running")
        return(invisible(NULL))
      }

      self$is_playing <- TRUE
      self$current_bar <- 0L

      # Start file watching
      self$start_watching()

      # Generate and start playback
      private$start_loop()

      invisible(NULL)
    },

    #' Stop Playback
    #'
    #' Stops playback immediately. Sets is_playing to FALSE, stops audio,
    #' and clears any pending later() callbacks.
    #'
    #' @return NULL (invisibly)
    stop = function() {
      self$is_playing <- FALSE

      # Stop file watching
      self$stop_watching()

      # Stop current audio
      stop_audio()

      # Clear pending callbacks
      if (!is.null(private$later_handle)) {
        tryCatch(
          later::later_cancel(private$later_handle),
          error = function(e) NULL
        )
        private$later_handle <- NULL
      }

      if (!is.null(private$playback_handle)) {
        tryCatch(
          later::later_cancel(private$playback_handle),
          error = function(e) NULL
        )
        private$playback_handle <- NULL
      }

      # Clear buffers
      private$buffer_queue <- list()
      private$current_buffer <- NULL
      private$next_buffer <- NULL
      private$current_instance <- NULL

      invisible(NULL)
    },

    #' Reload Script
    #'
    #' Re-analyzes the script file. On parse error, keeps last_known_good_model

    #' and sets pending_transition for potential glitch effect. On success,
    #' updates both code_model and last_known_good_model.
    #'
    #' @return Logical TRUE if reload succeeded, FALSE on parse error
    reload_script = function() {
      # Try to re-analyze script
      new_model <- tryCatch(
        raver_analyze(self$script_path),
        error = function(e) NULL,
        warning = function(w) {
          # Parse warnings indicate syntax errors
          NULL
        }
      )

      if (!is.null(new_model) && length(new_model$functions) >= 0) {
        # Successful parse - check if file actually changed
        if (is.null(self$code_model) ||
            !identical(new_model$file_hash, self$code_model$file_hash)) {
          self$code_model <- new_model
          self$last_known_good_model <- new_model
          self$pending_transition <- TRUE
          return(TRUE)
        }
        return(TRUE)  # No change needed
      } else {
        # Parse error - keep last known good model
        # pending_transition already set for glitch indication
        self$pending_transition <- TRUE
        return(FALSE)
      }
    },

    #' Queue Transition
    #'
    #' Marks a pending transition to be handled at the next phrase boundary.
    #' This allows smooth transitions without abrupt audio changes.
    #'
    #' @return NULL (invisibly)
    queue_transition = function() {
      self$pending_transition <- TRUE
      invisible(NULL)
    },

    #' Start File Watcher
    #'
    #' Creates and starts a watcher for the script file. When the file changes,
    #' on_file_change() is called to handle reloading and transition queueing.
    #'
    #' @return NULL (invisibly)
    start_watching = function() {
      # Get directory containing the script
      watch_dir <- dirname(self$script_path)
      target_file <- basename(self$script_path)

      # Create watcher with callback
      private$file_watcher <- watcher::watcher(
        path = watch_dir,
        callback = function(changed_files) {
          self$on_file_change(changed_files)
        },
        latency = 0.5  # 500ms debounce
      )

      # Start watching
      private$file_watcher$start()

      invisible(NULL)
    },

    #' Stop File Watcher
    #'
    #' Stops the file watcher if it is running and cleans up the reference.
    #'
    #' @return NULL (invisibly)
    stop_watching = function() {
      if (!is.null(private$file_watcher)) {
        tryCatch(
          private$file_watcher$stop(),
          error = function(e) NULL
        )
        private$file_watcher <- NULL
      }
      invisible(NULL)
    },

    #' Handle File Change
    #'
    #' Called by the watcher when file changes are detected. Checks if our
    #' script is among the changed files and triggers reload/transition.
    #' On parse error, sets has_parse_error flag for glitch effect generation.
    #'
    #' @param changed_files Character vector of changed file paths
    #'
    #' @return NULL (invisibly)
    on_file_change = function(changed_files) {
      # Check if our script is in the changed files
      script_basename <- basename(self$script_path)
      changed_basenames <- basename(changed_files)

      if (script_basename %in% changed_basenames) {
        # Try to reload the script
        old_model <- self$code_model
        parse_had_warning <- FALSE

        # Use withCallingHandlers to capture parse warnings (which indicate syntax errors)
        new_model <- tryCatch(
          withCallingHandlers(
            raver_analyze(self$script_path),
            warning = function(w) {
              # Check if it's a parse error warning
              if (grepl("Parse error", conditionMessage(w), ignore.case = TRUE)) {
                parse_had_warning <<- TRUE
              }
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            parse_had_warning <<- TRUE
            NULL
          }
        )

        if (parse_had_warning || is.null(new_model)) {
          # Parse error - keep old model, just notify
          self$has_parse_error <- TRUE
          warning("Parse error in script - keeping previous version")
        } else {
          # Check if file actually changed
          if (is.null(old_model) ||
              !identical(new_model$file_hash, old_model$file_hash)) {
            self$code_model <- new_model
            self$last_known_good_model <- new_model
            self$has_parse_error <- FALSE
            self$pending_transition <- TRUE

            # Pre-generate the new buffer while current loop finishes
            private$next_buffer <- private$generate_new_buffer()
            message("Script updated - will transition after current loop")
          }
        }
      }

      invisible(NULL)
    },

    #' Get State
    #'
    #' Returns current controller state for debugging and monitoring.
    #'
    #' @return A list with current state information
    get_state = function() {
      list(
        is_playing = self$is_playing,
        current_bar = self$current_bar,
        pending_transition = self$pending_transition,
        has_parse_error = self$has_parse_error,
        script_path = self$script_path,
        bpm = self$bpm,
        has_code_model = !is.null(self$code_model),
        has_fallback_model = !is.null(self$last_known_good_model),
        is_watching = !is.null(private$file_watcher)
      )
    }
  ),

  private = list(
    #' @field buffer_queue List of pre-rendered Wave objects awaiting playback
    buffer_queue = list(),

    #' @field current_instance Current audioInstance from play_audio()
    current_instance = NULL,

    #' @field later_handle Handle to cancel pending generation callbacks
    later_handle = NULL,

    #' @field playback_handle Handle to cancel pending playback callbacks
    playback_handle = NULL,

    #' @field file_watcher Watcher object for script file monitoring
    file_watcher = NULL,

    #' @field fade_samples Number of samples for crossfade (50ms at 44100Hz)
    fade_samples = 2205L,

    #' @field loop_bars Number of bars per loop (longer = fewer switches)
    loop_bars = 32L,

    #' @field current_buffer Current playing buffer for seamless re-looping
    current_buffer = NULL,

    #' @field next_buffer Pre-generated buffer for after transition
    next_buffer = NULL,

    #' Apply fade in/out to buffer for smooth transitions
    #'
    #' @param wave Wave object to apply fades to
    #' @param fade_in Apply fade in at start
    #' @param fade_out Apply fade out at end
    #' @return Wave object with fades applied
    apply_buffer_fades = function(wave, fade_in = TRUE, fade_out = TRUE) {
      samples <- wave@left
      n <- length(samples)
      fade_len <- min(private$fade_samples, n %/% 4)  # Don't fade more than 25% of buffer

      if (fade_in && fade_len > 0) {
        # Fade in: 0 to 1 over fade_len samples (raised cosine for smooth curve)
        fade_in_curve <- (1 - cos(seq(0, pi, length.out = fade_len))) / 2
        samples[seq_len(fade_len)] <- samples[seq_len(fade_len)] * fade_in_curve
      }

      if (fade_out && fade_len > 0) {
        # Fade out: 1 to 0 over fade_len samples
        fade_out_curve <- (1 + cos(seq(0, pi, length.out = fade_len))) / 2
        start_idx <- n - fade_len + 1
        samples[start_idx:n] <- samples[start_idx:n] * fade_out_curve
      }

      tuneR::Wave(
        left = samples,
        samp.rate = wave@samp.rate,
        bit = wave@bit,
        pcm = FALSE
      )
    },

    #' Generate new buffer from updated code model
    #'
    #' Pre-generates buffer while current loop finishes playing
    generate_new_buffer = function() {
      buffer <- self$generate_buffer("drop", private$loop_bars)
      private$apply_buffer_fades(buffer, fade_in = TRUE, fade_out = TRUE)
    },

    #' Start the playback loop
    #'
    #' Generates buffer and starts looping playback
    start_loop = function() {
      if (!self$is_playing) return(invisible(NULL))

      # Generate a long buffer (32 bars = ~64 seconds)
      buffer <- self$generate_buffer("drop", private$loop_bars)
      buffer <- private$apply_buffer_fades(buffer, fade_in = TRUE, fade_out = TRUE)

      # Store current buffer for re-looping
      private$current_buffer <- buffer

      # Start playback
      private$current_instance <- play_audio(buffer)

      # Schedule loop restart at end of buffer
      buffer_duration_sec <- private$calculate_buffer_duration(private$loop_bars)

      private$playback_handle <- later::later(
        function() private$continue_loop(),
        delay = buffer_duration_sec
      )

      invisible(NULL)
    },

    #' Continue the loop or transition if file changed
    #'
    #' If pending_transition, pause for one bar then play pre-generated buffer.
    #' Otherwise replay same buffer.
    continue_loop = function() {
      if (!self$is_playing) return(invisible(NULL))

      if (self$pending_transition) {
        # File changed - pause for one bar, then play new buffer
        self$pending_transition <- FALSE
        self$has_parse_error <- FALSE

        # One bar pause duration
        one_bar_sec <- private$calculate_buffer_duration(1L)

        # Schedule playing the pre-generated buffer after pause
        private$playback_handle <- later::later(
          function() private$play_transitioned_buffer(),
          delay = one_bar_sec
        )
      } else {
        # Replay same buffer
        private$current_instance <- play_audio(private$current_buffer)

        # Schedule next loop
        buffer_duration_sec <- private$calculate_buffer_duration(private$loop_bars)
        private$playback_handle <- later::later(
          function() private$continue_loop(),
          delay = buffer_duration_sec
        )
      }

      invisible(NULL)
    },

    #' Play the pre-generated transition buffer
    #'
    #' Called after the pause to play the new buffer
    play_transitioned_buffer = function() {
      if (!self$is_playing) return(invisible(NULL))

      # Use pre-generated buffer or generate fresh if not available
      if (!is.null(private$next_buffer)) {
        private$current_buffer <- private$next_buffer
        private$next_buffer <- NULL
      } else {
        private$current_buffer <- private$generate_new_buffer()
      }

      # Play the new buffer
      private$current_instance <- play_audio(private$current_buffer)

      # Schedule next loop
      buffer_duration_sec <- private$calculate_buffer_duration(private$loop_bars)
      private$playback_handle <- later::later(
        function() private$continue_loop(),
        delay = buffer_duration_sec
      )

      invisible(NULL)
    },

    #' Calculate Buffer Duration
    #'
    #' Calculates the duration of a buffer in seconds based on BPM and bar count.
    #'
    #' @param bars Number of bars
    #' @return Duration in seconds
    calculate_buffer_duration = function(bars) {
      # 4 beats per bar, duration = bars * 4 * (60 / bpm)
      bars * 4 * (60 / self$bpm)
    },

    #' Generate Transition Buffers
    #'
    #' Creates a sequence of buffers for a musical transition:
    #' breakdown -> build -> new drop. Each section is 4 bars
    #' (approximately 8 seconds at 120 BPM).
    #'
    #' @return A list of Wave objects for the transition sequence
    generate_transition_buffers = function() {
      buffers <- list()

      # Breakdown: stripped down, just pads and hats
      buffers$breakdown <- tryCatch(
        raver_compose_section(
          code_model = self$code_model,
          section_type = "breakdown",
          bpm = self$bpm,
          bars = 4L
        ),
        error = function(e) NULL
      )

      # Build: elements adding back, energy rising
      buffers$build <- tryCatch(
        raver_compose_section(
          code_model = self$code_model,
          section_type = "build",
          bpm = self$bpm,
          bars = 4L
        ),
        error = function(e) NULL
      )

      # New drop: full energy with updated code model
      buffers$drop <- tryCatch(
        raver_compose_section(
          code_model = self$code_model,
          section_type = "drop",
          bpm = self$bpm,
          bars = 4L
        ),
        error = function(e) NULL
      )

      # Filter out any NULL buffers from errors
      buffers <- buffers[!vapply(buffers, is.null, logical(1))]

      # Return as unnamed list for queue processing
      unname(buffers)
    }
  )
)
