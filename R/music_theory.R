#' @title Music Theory Functions
#' @description Core music theory calculations for deep house generation.
#'   Includes MIDI/frequency conversion, scale patterns, and chord formulas.
#' @name music-theory
NULL

# =============================================================================
# Constants: Scale Patterns
# =============================================================================

#' Scale interval patterns (semitones from root)
#'
#' @description Named list of scale patterns used in deep house production.
#'   Each pattern is a numeric vector of semitone offsets from the root note.
#' @export
SCALE_PATTERNS <- list(
  major = c(0L, 2L, 4L, 5L, 7L, 9L, 11L),
  natural_minor = c(0L, 2L, 3L, 5L, 7L, 8L, 10L),
  harmonic_minor = c(0L, 2L, 3L, 5L, 7L, 8L, 11L),
  dorian = c(0L, 2L, 3L, 5L, 7L, 9L, 10L),
  minor_pentatonic = c(0L, 3L, 5L, 7L, 10L),
  major_pentatonic = c(0L, 2L, 4L, 7L, 9L)
)

# =============================================================================
# Constants: Chord Formulas
# =============================================================================

#' Chord interval formulas (semitones from root)
#'
#' @description Named list of chord formulas used in deep house production.
#'   Each formula is a numeric vector of semitone offsets from the root note.
#' @export
CHORD_FORMULAS <- list(
  major = c(0L, 4L, 7L),
  minor = c(0L, 3L, 7L),
  maj7 = c(0L, 4L, 7L, 11L),
  min7 = c(0L, 3L, 7L, 10L),
  dom7 = c(0L, 4L, 7L, 10L),
  maj9 = c(0L, 4L, 7L, 11L, 14L),
  min9 = c(0L, 3L, 7L, 10L, 14L),
  dom9 = c(0L, 4L, 7L, 10L, 14L)
)

# =============================================================================
# Constants: Common House Keys
# =============================================================================

#' Common deep house root notes (MIDI numbers)
#'
#' @description Named list of root notes commonly used in deep house.
#'   Values are MIDI note numbers for the third octave (bass range).
#' @export
HOUSE_KEYS <- list(
  A_minor = 57L,   # A3
  C_major = 48L,   # C3
  G_minor = 55L,   # G3

  D_minor = 50L,   # D3
  F_major = 53L    # F3
)

# =============================================================================
# Note Offsets (internal)
# =============================================================================

# Base semitone offset for each natural note (C = 0)
.NOTE_OFFSETS <- c(
  C = 0L, D = 2L, E = 4L, F = 5L, G = 7L, A = 9L, B = 11L
)

# =============================================================================
# MIDI and Frequency Conversion
# =============================================================================

#' Convert MIDI note number to frequency in Hz
#'
#' @description Converts a MIDI note number to its corresponding frequency
#'   using A4 = 440 Hz as the reference pitch (A4 = MIDI 69).
#'
#' @param midi_note Integer or numeric MIDI note number (0-127 typical range)
#'
#' @return Numeric frequency in Hz
#'
#' @details Uses the standard equal temperament formula:
#'   freq = 440 * 2^((midi_note - 69) / 12)
#'
#' @examples
#' raver_midi_to_freq(69)  # A4 = 440 Hz
#' raver_midi_to_freq(60)  # C4 = ~261.63 Hz
#' raver_midi_to_freq(57)  # A3 = 220 Hz
#'
#' @export
raver_midi_to_freq <- function(midi_note) {
  440 * 2^((midi_note - 69) / 12)
}

#' Convert note name and octave to MIDI note number
#'
#' @description Converts a note name (e.g., "C", "F#", "Bb") and octave
#'   to its corresponding MIDI note number.
#'
#' @param note Character note name. Supports:
#'   - Natural notes: C, D, E, F, G, A, B
#'   - Sharps: C#, Cs, D#, Ds, F#, Fs, G#, Gs, A#, As
#'   - Flats: Db, Df, Eb, Ef, Gb, Gf, Ab, Af, Bb, Bf
#' @param octave Integer octave number (0-10 typical range)
#'
#' @return Integer MIDI note number
#'
#' @details MIDI note numbering:
#'   - C-1 = 0, C0 = 12, C4 = 60 (middle C), A4 = 69
#'   - Formula: (octave + 1) * 12 + note_offset + modifier
#'
#' @examples
#' raver_note_to_midi("C", 4)   # 60 (middle C)
#' raver_note_to_midi("A", 4)   # 69 (concert A)
#' raver_note_to_midi("C#", 4)  # 61
#' raver_note_to_midi("Bb", 4)  # 70
#'
#' @export
raver_note_to_midi <- function(note, octave) {
  # Parse note name
  note <- toupper(note)

  # Extract base note (first character)
  base_note <- substr(note, 1, 1)

  if (!(base_note %in% names(.NOTE_OFFSETS))) {
    stop("Invalid note name: ", note, ". Must be one of: C, D, E, F, G, A, B")
  }

  base_offset <- .NOTE_OFFSETS[[base_note]]

  # Check for modifier (sharp or flat)
  modifier <- 0L
  if (nchar(note) > 1) {
    mod_char <- substr(note, 2, 2)
    if (mod_char %in% c("#", "S")) {
      modifier <- 1L
    } else if (mod_char %in% c("B", "F")) {
      modifier <- -1L
    }
  }

  # Calculate MIDI number
  as.integer((octave + 1) * 12 + base_offset + modifier)
}

#' Convert frequency to nearest MIDI note number
#'
#' @description Converts a frequency in Hz to the nearest MIDI note number.
#'   Useful for debugging and analysis.
#'
#' @param freq Numeric frequency in Hz
#'
#' @return Integer nearest MIDI note number
#'
#' @details Uses the inverse of the MIDI-to-frequency formula:
#'   midi = 69 + 12 * log2(freq / 440)
#'
#' @examples
#' raver_freq_to_midi(440)  # 69 (A4)
#' raver_freq_to_midi(261.63)  # 60 (C4)
#'
#' @export
raver_freq_to_midi <- function(freq) {
  as.integer(round(69 + 12 * log2(freq / 440)))
}

# =============================================================================
# Scale and Chord Construction
# =============================================================================

#' Build a scale from a root note
#'
#' @description Builds a scale from a root MIDI note using a specified pattern.
#'   Returns MIDI note numbers spanning the requested number of octaves.
#'
#' @param root_midi Integer MIDI note number for the root
#' @param pattern Character name of scale pattern from SCALE_PATTERNS,
#'   or numeric vector of semitone intervals. Default "natural_minor".
#' @param octaves Integer number of octaves to span. Default 2.
#'
#' @return Integer vector of MIDI note numbers in the scale
#'
#' @examples
#' raver_build_scale(57, "natural_minor", 2)  # A minor, 2 octaves
#' raver_build_scale(48, "major", 1)          # C major, 1 octave
#'
#' @export
raver_build_scale <- function(root_midi, pattern = "natural_minor", octaves = 2) {
  # Get pattern intervals
  if (is.character(pattern)) {
    if (!(pattern %in% names(SCALE_PATTERNS))) {
      stop("Unknown scale pattern: ", pattern,
           ". Available: ", paste(names(SCALE_PATTERNS), collapse = ", "))
    }
    intervals <- SCALE_PATTERNS[[pattern]]
  } else {
    intervals <- as.integer(pattern)
  }

  # Build scale across octaves
  notes <- integer(0)
  for (oct in seq_len(octaves)) {
    octave_offset <- (oct - 1L) * 12L
    notes <- c(notes, root_midi + intervals + octave_offset)
  }

  as.integer(notes)
}

#' Build a chord from a root note
#'
#' @description Builds a chord from a root MIDI note using a specified formula.
#'   Returns MIDI note numbers for the chord.
#'
#' @param root_midi Integer MIDI note number for the root
#' @param chord_type Character name of chord type from CHORD_FORMULAS,
#'   or numeric vector of semitone intervals. Default "min9".
#'
#' @return Integer vector of MIDI note numbers in the chord
#'
#' @examples
#' raver_build_chord(48, "min9")   # Cmin9 = C, Eb, G, Bb, D
#' raver_build_chord(57, "min7")   # Amin7 = A, C, E, G
#'
#' @export
raver_build_chord <- function(root_midi, chord_type = "min9") {
  # Get chord intervals
  if (is.character(chord_type)) {
    if (!(chord_type %in% names(CHORD_FORMULAS))) {
      stop("Unknown chord type: ", chord_type,
           ". Available: ", paste(names(CHORD_FORMULAS), collapse = ", "))
    }
    intervals <- CHORD_FORMULAS[[chord_type]]
  } else {
    intervals <- as.integer(chord_type)
  }

  as.integer(root_midi + intervals)
}

#' Invert a chord for voice leading
#'
#' @description Creates a chord inversion by moving the bottom N notes
#'   up an octave. Used for smooth voice leading between chords.
#'
#' @param chord_notes Integer vector of MIDI note numbers
#' @param inversion Integer inversion number.
#'   0 = root position, 1 = first inversion, 2 = second inversion, etc.
#'   Wraps around if inversion > chord length.
#'
#' @return Integer vector of MIDI note numbers in the inverted chord
#'
#' @examples
#' raver_invert_chord(c(48, 51, 55), 1)  # Move C up: Eb, G, C
#' raver_invert_chord(c(48, 51, 55), 2)  # Move C, Eb up: G, C, Eb
#'
#' @export
raver_invert_chord <- function(chord_notes, inversion = 0) {
  if (inversion == 0) {
    return(as.integer(chord_notes))
  }

  chord_notes <- as.integer(chord_notes)
  n <- length(chord_notes)

  # Wrap inversion to valid range
  inversion <- inversion %% n
  if (inversion == 0) {
    return(chord_notes)
  }

  # Move bottom 'inversion' notes up an octave
  bottom <- chord_notes[seq_len(inversion)]
  top <- chord_notes[(inversion + 1):n]

  as.integer(c(top, bottom + 12L))
}

#' Get chord frequencies
#'
#' @description Convenience function that builds a chord and returns
#'   frequencies in Hz instead of MIDI note numbers.
#'
#' @param root_midi Integer MIDI note number for the root
#' @param chord_type Character name of chord type from CHORD_FORMULAS.
#'   Default "min9".
#'
#' @return Numeric vector of frequencies in Hz
#'
#' @examples
#' raver_get_chord_freqs(48, "min9")  # Cmin9 frequencies
#'
#' @export
raver_get_chord_freqs <- function(root_midi, chord_type = "min9") {
  chord_notes <- raver_build_chord(root_midi, chord_type)
  raver_midi_to_freq(chord_notes)
}
