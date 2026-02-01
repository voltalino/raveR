#' @title Melodic Hooks from Function Names
#' @description Maps function names to melodic pitches for memorable,
#'   recognizable musical themes. Each function gets a unique motif based
#'   on its name hash.
#' @name melodic_hooks
NULL

# =============================================================================
# Function Name to Pitch Mapping
# =============================================================================

#' Map Function Name to Pitch Sequence
#'
#' @description Converts a function name to a deterministic pitch sequence
#'   using character hashing. Same function name always produces same melody.
#'
#' @param func_name Character function name
#' @param root_note Integer MIDI note for root (0-127). Default 60 (C4).
#' @param scale_type Character: "major", "minor", "pentatonic". Default "minor".
#'
#' @return Integer vector of MIDI note numbers (melody pitches)
#' @keywords internal
func_name_to_pitches <- function(func_name, root_note = 60, scale_type = "minor") {
  if (nchar(func_name) == 0) {
    return(c(root_note, root_note + 3, root_note + 7))  # Default minor triad
  }

  # Hash the function name for determinism
  hash <- digest::digest(func_name, algo = "md5")
  hash_nums <- as.integer(strtoi(substr(hash, 1:8, 1:8), base = 16)) %% 16

  # Define scales
  scales <- list(
    major = c(0, 2, 4, 5, 7, 9, 11),
    minor = c(0, 2, 3, 5, 7, 8, 10),
    pentatonic = c(0, 2, 4, 7, 9)
  )

  scale <- scales[[scale_type]]

  # Generate 4-note melody from hash
  melody <- sapply(hash_nums[1:4], function(n) {
    octave <- (n %/% length(scale)) - 1  # -1, 0, or 1 octave offset
    scale_idx <- (n %% length(scale)) + 1
    root_note + scale[scale_idx] + (octave * 12)
  })

  # Ensure valid MIDI range
  pmin(pmax(melody, 0), 127)
}

#' Generate Lead Motif from Function
#'
#' @description Creates a lead synthesizer motif based on function name.
#'   Returns note sequence with durations for the sequencer.
#'
#' @param func_name Character function name
#' @param bpm Numeric tempo
#' @param intensity Numeric 0-1 (complexity drives this)
#'
#' @return Data frame with note, velocity, duration_sec, start_beat
#' @keywords internal
generate_lead_motif <- function(func_name, bpm, intensity = 0.5) {
  pitches <- func_name_to_pitches(func_name)

  # Duration based on intensity (higher complexity = faster notes)
  base_duration <- 60 / bpm  # One beat in seconds
  durations <- rep(base_duration * (0.5 + intensity), length(pitches))

  # Velocity variation for human feel
  velocities <- 80 + (pitches %% 20)

  # Build sequence
  data.frame(
    note = pitches,
    velocity = velocities,
    duration_sec = durations,
    start_beat = cumsum(c(0, durations[-length(durations)])) / base_duration
  )
}

#' Generate Bass Line from Variable Name
#'
#' @description Creates a bass line motif from variable name.
#'   Lower octave, rhythmic emphasis on root and fifth.
#'
#' @param var_name Character variable name
#' @param root_note Integer MIDI note for harmonic root
#' @param bpm Numeric tempo
#'
#' @return Data frame with note, velocity, duration_sec, start_beat
#' @keywords internal
generate_bass_motif <- function(var_name, root_note = 48, bpm = 120) {
  if (nchar(var_name) == 0) {
    # Default root-fifth pattern
    notes <- c(root_note, root_note, root_note + 7, root_note)
  } else {
    # Hash variable name to bass pattern
    hash <- digest::digest(var_name, algo = "md5")
    pattern_idx <- strtoi(substr(hash, 1, 2), base = 16) %% 4

    # Bass patterns (scale degrees in minor)
    patterns <- list(
      c(0, 0, 7, 0),      # Root-fifth
      c(0, 3, 0, 7),      # Root-third-fifth
      c(0, 7, 3, 7),      # Alternating
      c(0, 0, 0, 7)       # Sparse
    )

    intervals <- patterns[[pattern_idx + 1]]
    notes <- root_note + intervals
  }

  # One bar bass line (4 beats)
  beat_duration <- 60 / bpm
  durations <- rep(beat_duration, 4)

  data.frame(
    note = notes,
    velocity = rep(100, 4),
    duration_sec = durations,
    start_beat = 0:3
  )
}

#' Build Hook Library from Code Model
#'
#' @description Creates a library of melodic hooks for all functions
#'   and variables in the code model.
#'
#' @param code_model CodeModel object
#' @param bpm Numeric tempo
#'
#' @return List with function_hooks and variable_hooks
#' @export
build_hook_library <- function(code_model, bpm) {
  # Generate hooks for each function
  function_hooks <- lapply(code_model$functions, function(func) {
    intensity <- min(length(code_model$functions) / 10, 1.0)
    generate_lead_motif(func, bpm, intensity)
  })
  names(function_hooks) <- code_model$functions

  # Generate bass motifs for variables
  variable_hooks <- lapply(code_model$variables, function(var) {
    generate_bass_motif(var, bpm = bpm)
  })
  names(variable_hooks) <- code_model$variables

  list(
    function_hooks = function_hooks,
    variable_hooks = variable_hooks,
    bpm = bpm
  )
}
