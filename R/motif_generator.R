#' @title Motif Generator for Function-Specific Melodies
#' @description Generate deterministic melodic motifs from function names using
#'   MD5 hashing. The same function name always produces the same motif,
#'   ensuring consistent musical identity across sessions.
#' @name motif-generator
NULL

# =============================================================================
# Motif Generation
# =============================================================================

#' Generate a melodic motif from a name
#'
#' @description Creates a unique melodic motif from any string (function name,
#'   variable name, etc.) using MD5 hashing for determinism. The same name
#'   always produces the same motif.
#'
#' @param name Character string to generate motif from (e.g., function name)
#' @param scale_notes Integer vector of MIDI notes in the scale (from
#'   raver_build_scale())
#' @param seed Optional character string to append to name for variation.
#'   Default NULL.
#' @param length Integer target length of the motif (4-8 notes). The actual
#'   length is determined by the hash but capped at this value. Default 8.
#'
#' @return List with elements:
#'   \itemize{
#'     \item name: Original name used for generation
#'     \item rhythm: Logical vector (16 steps, TRUE where notes occur)
#'     \item notes: Integer vector of MIDI notes from scale
#'     \item velocities: Numeric vector of velocities (0.7-1.0)
#'     \item seed: Seed used (or NULL)
#'   }
#'
#' @details
#' The motif is extracted from the MD5 hash as follows:
#' - Bytes 1-2: 16-bit rhythm pattern (which steps have notes)
#' - Bytes 3-10: Note indices (scale degree per step, mod scale length)
#' - Bytes 11-14: Velocities (mapped to 0.7-1.0 range)
#' - Byte 15: Phrase length modifier (4-8 notes)
#'
#' @examples
#' \dontrun{
#' scale <- raver_build_scale(57, "natural_minor", 2)
#' motif <- generate_motif("clean_data", scale)
#' motif$rhythm   # 16-step pattern
#' motif$notes    # MIDI notes for active steps
#' }
#'
#' @export
generate_motif <- function(name, scale_notes, seed = NULL, length = 8) {
  # Combine name with seed if provided

hash_input <- if (!is.null(seed)) paste0(name, seed) else name

  # Generate MD5 hash and extract bytes
  hash_str <- digest::digest(hash_input, algo = "md5")
  bytes <- strtoi(substring(hash_str, seq(1, 31, 2), seq(2, 32, 2)), base = 16L)

  # Extract rhythm pattern from bytes 1-2 (16 bits = 16 steps)
  rhythm_bits <- bitwOr(bitwShiftL(bytes[1], 8L), bytes[2])
  rhythm <- vapply(0:15, function(i) bitwAnd(bitwShiftR(rhythm_bits, i), 1L) == 1L, logical(1))

  # Ensure at least 2 notes in rhythm (avoid empty patterns)
  if (sum(rhythm) < 2) {
    # Force first and fifth steps to be active (common bass pattern)
    rhythm[1] <- TRUE
    rhythm[5] <- TRUE
  }

  # Determine phrase length from byte 15
  phrase_length <- (bytes[15] %% 4L) + 4L  # 4-8 notes
  phrase_length <- min(phrase_length, length)

  # Limit active steps to phrase length
  active_indices <- which(rhythm)
  if (length(active_indices) > phrase_length) {
    rhythm[active_indices[(phrase_length + 1):length(active_indices)]] <- FALSE
  }

  # Extract note indices from bytes 3-10 (8 bytes for up to 8 notes)
  note_indices <- (bytes[3:10] %% length(scale_notes)) + 1L
  active_count <- sum(rhythm)
  note_indices <- note_indices[seq_len(active_count)]

  # Get actual MIDI notes from scale
  notes <- scale_notes[note_indices]

  # Extract velocities from bytes 11-14, map to 0.7-1.0 range
  raw_velocities <- bytes[11:14]
  velocities_base <- (raw_velocities / 255) * 0.3 + 0.7  # 0.7 to 1.0

  # Extend velocities if needed
  if (active_count > 4) {
    velocities <- rep(velocities_base, length.out = active_count)
  } else {
    velocities <- velocities_base[seq_len(active_count)]
  }

  list(
    name = name,
    rhythm = rhythm,
    notes = as.integer(notes),
    velocities = as.numeric(velocities),
    seed = seed
  )
}

#' Convert motif to sequencer pattern
#'
#' @description Converts a motif to a pattern compatible with the step sequencer.
#'   Maps motif notes to pattern steps.
#'
#' @param motif A motif list from generate_motif()
#' @param instrument Character string identifying instrument. Default "bass".
#' @param duration_16ths Integer note length in 16th notes. 1 = 16th, 2 = 8th,
#'   4 = quarter. Default 2.
#'
#' @return A pattern list compatible with create_pattern()
#'
#' @details
#' The motif's rhythm determines which steps are active. The first note in
#' the motif is used as the pattern's note parameter (for monophonic instruments).
#' Velocities are applied per step.
#'
#' @examples
#' \dontrun{
#' scale <- raver_build_scale(57, "natural_minor", 2)
#' motif <- generate_motif("clean_data", scale)
#' pattern <- motif_to_pattern(motif, "bass")
#' }
#'
#' @export
motif_to_pattern <- function(motif, instrument = "bass", duration_16ths = 2) {
  # Build step velocities from motif
  steps <- numeric(16)
  active_indices <- which(motif$rhythm)

  for (i in seq_along(active_indices)) {
    step_idx <- active_indices[i]
    if (i <= length(motif$velocities)) {
      steps[step_idx] <- motif$velocities[i]
    } else {
      steps[step_idx] <- 0.8  # Default velocity if not enough
    }
  }

  # Use first note as the pattern note (monophonic)
  note <- if (length(motif$notes) > 0) motif$notes[1] else 57L  # Default A3

  list(
    steps = steps,
    instrument = instrument,
    steps_per_bar = 16L,
    note = note,
    velocity_base = 1.0,
    swing = 0.0
  )
}

#' Evolve a motif with subtle variations
#'
#' @description Creates a subtle variation of an existing motif for generative
#'   looping. Uses deterministic variation based on the original motif.
#'
#' @param motif A motif list from generate_motif()
#' @param variation Numeric variation amount from 0.0 to 1.0. Higher values
#'   mean more changes. Default 0.1.
#'
#' @return A new motif list with subtle variations
#'
#' @details
#' Variation is applied deterministically using a hash of the original motif
#' plus "evolve" suffix. Changes include:
#' - Rhythm: 5% chance per step of flip (scaled by variation)
#' - Notes: 10% chance of shift by +/- 1 scale degree (scaled by variation)
#' - Velocities: Adjusted within 0.1 range (scaled by variation)
#'
#' @examples
#' \dontrun{
#' scale <- raver_build_scale(57, "natural_minor", 2)
#' motif <- generate_motif("clean_data", scale)
#' evolved <- evolve_motif(motif, 0.1)
#' }
#'
#' @export
evolve_motif <- function(motif, variation = 0.1) {
  # Generate deterministic variation using hash
  evolve_input <- paste0(motif$name, "evolve", motif$seed)
  hash_str <- digest::digest(evolve_input, algo = "md5")
  bytes <- strtoi(substring(hash_str, seq(1, 31, 2), seq(2, 32, 2)), base = 16L)

  # Clamp variation
  variation <- max(0.0, min(1.0, variation))

  # Evolve rhythm (5% base chance, scaled by variation)
  rhythm_chance <- 0.05 * variation
  new_rhythm <- motif$rhythm
  for (i in seq_along(new_rhythm)) {
    # Use hash bytes as deterministic random
    if ((bytes[(i %% 16) + 1] / 255) < rhythm_chance) {
      new_rhythm[i] <- !new_rhythm[i]
    }
  }

  # Ensure at least 2 notes remain
  if (sum(new_rhythm) < 2) {
    new_rhythm[1] <- TRUE
    new_rhythm[5] <- TRUE
  }

  # Evolve notes (10% base chance, scaled by variation)
  note_chance <- 0.10 * variation
  new_notes <- motif$notes
  for (i in seq_along(new_notes)) {
    byte_idx <- ((i + 8) %% 16) + 1
    if ((bytes[byte_idx] / 255) < note_chance) {
      # Shift by +/- 1 (use next byte for direction)
      direction <- if (bytes[(byte_idx %% 16) + 1] > 127) 1L else -1L
      new_notes[i] <- new_notes[i] + direction
    }
  }

  # Evolve velocities (within 0.1 range, scaled by variation)
  new_velocities <- motif$velocities
  for (i in seq_along(new_velocities)) {
    byte_idx <- ((i + 12) %% 16) + 1
    offset <- ((bytes[byte_idx] / 255) - 0.5) * 0.2 * variation
    new_velocities[i] <- max(0.7, min(1.0, new_velocities[i] + offset))
  }

  # Adjust notes list length if rhythm changed
  active_count <- sum(new_rhythm)
  if (active_count > length(new_notes)) {
    # Repeat notes if we need more
    new_notes <- rep(new_notes, length.out = active_count)
  } else if (active_count < length(new_notes)) {
    new_notes <- new_notes[seq_len(active_count)]
  }

  # Same for velocities
  if (active_count > length(new_velocities)) {
    new_velocities <- rep(new_velocities, length.out = active_count)
  } else if (active_count < length(new_velocities)) {
    new_velocities <- new_velocities[seq_len(active_count)]
  }

  list(
    name = motif$name,
    rhythm = new_rhythm,
    notes = as.integer(new_notes),
    velocities = as.numeric(new_velocities),
    seed = paste0(motif$seed, "_evolved")
  )
}

# =============================================================================
# Key and Progression Hashing
# =============================================================================

#' Deterministically select a house key from a name
#'
#' @description Hashes a name to deterministically select a root MIDI note
#'   from the common deep house keys.
#'
#' @param name Character string to hash (e.g., file path, function name)
#'
#' @return Integer MIDI note number for the selected key
#'
#' @examples
#' \dontrun{
#' key <- hash_to_key("my_data_pipeline.R")
#' # Always returns the same key for the same name
#' }
#'
#' @export
hash_to_key <- function(name) {
  hash_str <- digest::digest(name, algo = "md5")
  hash_int <- strtoi(substr(hash_str, 1, 7), base = 16L)

  # Get keys from HOUSE_KEYS constant
  key_names <- names(HOUSE_KEYS)
  selected_idx <- (hash_int %% length(key_names)) + 1L

  HOUSE_KEYS[[selected_idx]]
}

#' Generate a chord progression from a name
#'
#' @description Hashes a name to deterministically generate a chord progression
#'   for the track.
#'
#' @param name Character string to hash
#' @param length Integer number of chords in progression. Default 4.
#'
#' @return List of chord specs, each with root (scale degree) and type
#'
#' @details
#' The progression uses common deep house chord types: min7, maj7, dom7, min9.
#' Root notes are expressed as scale degrees (1-7) to be relative to the key.
#'
#' @examples
#' \dontrun{
#' prog <- hash_to_progression("clean_data")
#' # Returns list of chord specs like:
#' # list(list(root = 1, type = "min7"), list(root = 4, type = "maj7"), ...)
#' }
#'
#' @export
hash_to_progression <- function(name, length = 4) {
  hash_str <- digest::digest(name, algo = "md5")
  bytes <- strtoi(substring(hash_str, seq(1, 31, 2), seq(2, 32, 2)), base = 16L)

  # Common deep house chord types
  chord_types <- c("min7", "maj7", "dom7", "min9")

  progression <- lapply(seq_len(length), function(i) {
    # Use bytes for root and type selection (wrap around to stay in bounds)
    root_byte <- bytes[((i - 1) %% 16) + 1]
    type_byte <- bytes[((i + 7) %% 16) + 1]

    # Root: scale degree 1-7 (favor 1, 4, 5, 6 for house)
    root_weights <- c(1, 2, 3, 4, 4, 5, 5, 6, 6, 6, 7)
    root_idx <- (root_byte %% length(root_weights)) + 1L
    root <- root_weights[root_idx]

    # Chord type
    type_idx <- (type_byte %% length(chord_types)) + 1L
    chord_type <- chord_types[type_idx]

    list(root = root, type = chord_type)
  })

  progression
}
