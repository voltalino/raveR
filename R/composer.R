#' @title Main Composition Engine
#' @description The main composition engine that generates complete deep house
#'   tracks from code analysis. Integrates music theory, instruments, sequencer,
#'   motifs, and arrangement components.
#' @name composer
NULL

# =============================================================================
# BPM Handling
# =============================================================================

#' Select BPM from code model
#'
#' @description Deterministically selects a BPM within the 118-124 range
#'   based on the file path hash.
#'
#' @param code_model A CodeModel object
#'
#' @return Integer BPM between 118 and 124
#' @keywords internal
select_bpm <- function(code_model) {
  # Hash file path for determinism

  hash_str <- digest::digest(code_model$file_path, algo = "md5")
  hash_int <- strtoi(substr(hash_str, 1, 7), base = 16L)

  # Map to 118-124 range (7 values: 118, 119, 120, 121, 122, 123, 124)
  bpm <- 118L + (hash_int %% 7L)
  as.integer(bpm)
}

#' Validate BPM value
#'
#' @description Validates that BPM is within acceptable range (60-180).
#'
#' @param bpm Numeric BPM value to validate
#' @param genre Character genre for context
#'
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_bpm <- function(bpm, genre = "deep_house") {
  if (!is.numeric(bpm) || length(bpm) != 1) {
    stop("BPM must be a single numeric value", call. = FALSE)
  }

  # Extended range for all genres: 60-180
  if (bpm < 60 || bpm > 180) {
    stop(
      "BPM must be between 60 and 180. Got: ", bpm,
      "\n  Extreme tempos can cause synthesis instability.",
      call. = FALSE
    )
  }

  TRUE
}

# =============================================================================
# Genre Configuration
# =============================================================================

#' Genre Configuration
#'
#' @description Returns configuration parameters for each supported genre.
#'
#' @param genre Character: "deep_house", "techno", "ambient", "drum_bass"
#' @return List with genre-specific parameters
#' @keywords internal
get_genre_config <- function(genre) {
  configs <- list(
    deep_house = list(
      bpm_range = c(118, 124),
      drum_pattern = "four_on_floor",
      swing_amount = 0.15,
      bass_style = "filtered_deep",
      pad_style = "warm_chords",
      arrangement = "intro_build_drop_breakdown_outro"
    ),
    techno = list(
      bpm_range = c(125, 140),
      drum_pattern = "four_on_floor_hard",
      swing_amount = 0.05,
      bass_style = "acid_resonant",
      pad_style = "droning",
      arrangement = "linear_build"
    ),
    ambient = list(
      bpm_range = c(60, 90),
      drum_pattern = "sparse_textural",
      swing_amount = 0.0,
      bass_style = "subtle_pad",
      pad_style = "evolving_atmosphere",
      arrangement = "flowing"
    ),
    drum_bass = list(
      bpm_range = c(160, 180),
      drum_pattern = "breakbeat_fast",
      swing_amount = 0.25,
      bass_style = " Reese",
      pad_style = "stabs",
      arrangement = "drop_heavy"
    )
  )

  configs[[genre]]
}

#' Select BPM for Genre
#'
#' @description Deterministically selects BPM within genre-appropriate range.
#'
#' @param code_model A CodeModel object
#' @param genre Character genre name
#'
#' @return Integer BPM within genre range
#' @keywords internal
select_bpm_for_genre <- function(code_model, genre = "deep_house") {
  config <- get_genre_config(genre)
  bpm_range <- config$bpm_range

  hash_str <- digest::digest(code_model$file_path, algo = "md5")
  hash_int <- strtoi(substr(hash_str, 1, 7), base = 16L)

  # Map to genre-specific range
  range_size <- bpm_range[2] - bpm_range[1] + 1
  bpm <- bpm_range[1] + (hash_int %% range_size)
  as.integer(bpm)
}

# =============================================================================
# Main Composition Functions
# =============================================================================

#' Compose a complete track from code analysis
#'
#' @description Main composition function that generates a complete deep house
#'   track from a CodeModel. Integrates all components (instruments, sequencer,
#'   arrangement, motifs) into a coherent track.
#'
#' @param code_model CodeModel object from raver_analyze()
#' @param bpm Numeric tempo. If NULL, auto-selects based on file hash
#'   (always within 118-124 range). Default NULL.
#' @param seed Random seed for reproducibility. If NULL, uses file_hash
#'   for determinism. Default NULL.
#' @param sample_rate Integer sample rate in Hz. Default SAMPLE_RATE.
#'
#' @return A Wave object containing the complete track
#'
#' @details
#' The composition process:
#' 1. Set random seed for determinism
#' 2. Auto-select or validate BPM
#' 3. Create arrangement from code model
#' 4. Generate each section
#' 5. Concatenate sections
#' 6. Apply master processing (normalize, limit)
#'
#' @examples
#' \dontrun{
#' model <- raver_analyze("my_script.R")
#' track <- raver_compose(model, bpm = 122)
#' raver_export_wav(track, "output.wav")
#' }
#'
#' @export
raver_compose <- function(code_model, bpm = NULL, seed = NULL, sample_rate = SAMPLE_RATE, genre = "deep_house") {
  # Validate input
  if (!inherits(code_model, "CodeModel")) {
    stop("code_model must be a CodeModel object", call. = FALSE)
  }

  # Validate genre
  valid_genres <- c("deep_house", "techno", "ambient", "drum_bass")
  if (!genre %in% valid_genres) {
    stop("Invalid genre: ", genre, ". Valid: ", paste(valid_genres, collapse = ", "), call. = FALSE)
  }

  # Save current RNG state to restore later (handle case where .Random.seed doesn't exist)
  has_old_seed <- exists(".Random.seed", envir = globalenv())
  if (has_old_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
  }
  on.exit({
    if (has_old_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  # Set seed for determinism
  effective_seed <- if (!is.null(seed)) seed else code_model$file_hash
  set.seed(strtoi(substr(digest::digest(effective_seed), 1, 7), base = 16L))

  # Get genre configuration
  genre_config <- get_genre_config(genre)

  # Auto-select BPM if not provided (genre-specific range)
  if (is.null(bpm)) {
    bpm <- select_bpm_for_genre(code_model, genre)
  }
  validate_bpm(bpm, genre)

  # Create arrangement with genre and dynamic complexity response
  arrangement <- create_arrangement(code_model, bpm, genre_config)

  # Generate each section
  sections <- lapply(arrangement$sections, function(section) {
    render_section(section, arrangement, sample_rate)
  })

  # Concatenate sections
  if (length(sections) > 0) {
    track <- do.call(bind_waves, sections)
  } else {
    # Edge case: empty arrangement
    track <- create_silence(2, sample_rate)
  }

  # Master processing
  track <- normalize_mix(track, headroom_db = -3)
  track <- apply_master_limiter(track, threshold = 0.95)

  track
}

#' Compose a single section from code analysis
#'
#' @description Generates a single section of a track. Useful for testing,
#'   previewing, or generating shorter clips.
#'
#' @param code_model CodeModel object from raver_analyze()
#' @param section_type Character section type: "intro", "build", "drop",
#'   "breakdown", or "outro"
#' @param bpm Numeric tempo. Default 120.
#' @param bars Integer number of bars to generate. If NULL, uses default
#'   for section type. Default NULL.
#' @param sample_rate Integer sample rate in Hz. Default SAMPLE_RATE.
#'
#' @return A Wave object containing the section
#'
#' @details
#' This function is useful for:
#' - Testing specific section types
#' - Previewing how code maps to music
#' - Generating short clips for demos
#'
#' @examples
#' \dontrun{
#' model <- raver_analyze("my_script.R")
#' drop <- raver_compose_section(model, "drop", bpm = 122, bars = 8)
#' }
#'
#' @export
raver_compose_section <- function(code_model, section_type, bpm = 120,
                                   bars = NULL, sample_rate = SAMPLE_RATE) {
  # Validate input
  if (!inherits(code_model, "CodeModel")) {
    stop("code_model must be a CodeModel object", call. = FALSE)
  }

  if (!(section_type %in% names(SECTION_TYPES))) {
    stop(
      "Invalid section_type: ", section_type,
      ". Must be one of: ", paste(names(SECTION_TYPES), collapse = ", "),
      call. = FALSE
    )
  }

  validate_bpm(bpm)

  # Save and restore RNG state (handle case where .Random.seed doesn't exist)
  has_old_seed <- exists(".Random.seed", envir = globalenv())
  if (has_old_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
  }
  on.exit({
    if (has_old_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  # Set seed based on file hash for consistency
  set.seed(strtoi(substr(digest::digest(code_model$file_hash), 1, 7), base = 16L))

  # Create arrangement for context
  arrangement <- create_arrangement(code_model, bpm)

  # Get section template
  section_template <- SECTION_TYPES[[section_type]]

  # Override bars if specified
  section_bars <- if (!is.null(bars)) as.integer(bars) else section_template$bars

  # Build section spec
  section <- list(
    type = section_type,
    bars = section_bars,
    description = section_template$description,
    elements = section_template$elements,
    filter_start = section_template$filter_start,
    filter_end = section_template$filter_end
  )

  # Render section
  wave <- render_section(section, arrangement, sample_rate)

  # Master processing
  wave <- normalize_mix(wave, headroom_db = -3)
  wave <- apply_master_limiter(wave, threshold = 0.95)

  wave
}

# =============================================================================
# Internal Pattern Generation
# =============================================================================

#' Generate drum patterns for a section
#'
#' @description Internal helper that creates drum patterns based on section
#'   characteristics and density.
#'
#' @param section Section spec from arrangement
#' @param bpm Numeric beats per minute
#' @param density Numeric density level (0-1)
#'
#' @return List of pattern objects for drums
#' @keywords internal
generate_drum_patterns <- function(section, bpm, density) {
  elements <- section$elements
  patterns <- list()

  # Four-on-the-floor kick (always present in house)
  if ("kick" %in% elements) {
    kick_steps <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                    TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
    patterns$kick <- create_pattern("kick", kick_steps)
  }

  # Snare on 2 and 4
  if ("snare" %in% elements) {
    snare_steps <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                     FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
    patterns$snare <- create_pattern("snare", snare_steps)
  }

  # Clap on 2 and 4 (layer with snare)
  if ("clap" %in% elements) {
    clap_steps <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                    FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
    patterns$clap <- create_pattern("clap", clap_steps, velocity = 0.8)
  }

  # Hi-hats - density affects pattern
  if ("hihat_closed" %in% elements) {
    if (density > 0.7) {
      # Full 16th note hats for high density
      hihat_steps <- rep(TRUE, 16)
    } else if (density > 0.4) {
      # 8th note hats for medium density
      hihat_steps <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
                       TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    } else {
      # Offbeat 8ths for low density
      hihat_steps <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE,
                       FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
    }
    patterns$hihat_closed <- create_pattern("hihat_closed", hihat_steps, swing = 0.08)
  }

  # Open hi-hat for accents
  if ("hihat_open" %in% elements) {
    hihat_open_steps <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
                          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    patterns$hihat_open <- create_pattern("hihat_open", hihat_open_steps, velocity = 0.7)
  }

  patterns
}

#' Generate bass pattern from motif
#'
#' @description Internal helper that creates a bass pattern from a motif,
#'   with octave variations for movement.
#'
#' @param motif A motif from generate_motif()
#' @param section Section spec
#' @param bpm Numeric beats per minute
#'
#' @return A pattern object for bass
#' @keywords internal
generate_bass_pattern <- function(motif, section, bpm) {
  if (is.null(motif)) {
    # Default bass pattern if no motif
    bass_steps <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
                    TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
    return(create_pattern("bass", bass_steps, note = 45L))  # A2
  }

  # Convert motif to pattern
  motif_to_pattern(motif, "bass", duration_16ths = 2)
}

#' Generate pad progression for a section
#'
#' @description Internal helper that creates pad audio for a section using
#'   the arrangement key and hash-derived progression.
#'
#' @param arrangement Full arrangement object
#' @param section Section spec
#' @param bars Number of bars
#' @param sample_rate Sample rate
#'
#' @return A Wave object with pad progression
#' @keywords internal
generate_pad_progression <- function(arrangement, section, bars, sample_rate = SAMPLE_RATE) {
  bpm <- arrangement$bpm
  key <- arrangement$key

  # Calculate bar duration
  bar_duration_sec <- (60 / bpm) * 4

  # Build scale for root note calculation
  scale <- raver_build_scale(key, "natural_minor", 2)

  # Create chord duration pattern: mix of 4-bar, 2-bar, and 1-bar holds

  # Pattern for 32 bars: [4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2] = 32 bars
  # This creates sections of stability with occasional movement
  duration_pattern <- c(4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2)

  # Get unique chords needed (fewer than bars since some chords repeat)
  num_chords <- length(duration_pattern)
  prog_spec <- hash_to_progression(as.character(key), num_chords)

  # Generate chords with varied durations
  chord_waves <- list()
  bar_position <- 1

  for (i in seq_along(duration_pattern)) {
    if (bar_position > bars) break

    chord_duration_bars <- min(duration_pattern[i], bars - bar_position + 1)
    chord_spec <- prog_spec[[(i - 1) %% length(prog_spec) + 1]]

    # Get root from scale degree
    scale_idx <- chord_spec$root
    if (scale_idx > length(scale)) scale_idx <- ((scale_idx - 1) %% length(scale)) + 1
    root_midi <- scale[scale_idx]

    # Generate pad chord with extended duration
    chord_wave <- raver_pad_chord(
      root_midi = root_midi,
      chord_type = chord_spec$type,
      duration_sec = bar_duration_sec * chord_duration_bars,
      velocity = 0.7
    )

    chord_waves[[length(chord_waves) + 1]] <- chord_wave
    bar_position <- bar_position + chord_duration_bars
  }

  # Concatenate chords
  if (length(chord_waves) > 0) {
    do.call(bind_waves, chord_waves)
  } else {
    create_silence(bar_duration_sec * bars, sample_rate)
  }
}

#' Generate bass line following chord progression
#'
#' @description Internal helper that creates a bass line following the chord
#'   roots with rhythmic variation. Uses same chord duration pattern as pads.
#'
#' @param arrangement Full arrangement object
#' @param section Section spec
#' @param bars Number of bars
#' @param sample_rate Sample rate
#'
#' @return A Wave object with bass line
#' @keywords internal
generate_bass_line <- function(arrangement, section, bars, sample_rate = SAMPLE_RATE) {
  bpm <- arrangement$bpm
  key <- arrangement$key

  # Build scale for root note calculation
  scale <- raver_build_scale(key, "natural_minor", 2)

  # Same chord duration pattern as pads for consistency
  duration_pattern <- c(4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2)
  num_chords <- length(duration_pattern)
  prog_spec <- hash_to_progression(as.character(key), num_chords)

  # Calculate timing
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / 16
  total_samples <- as.integer(bar_duration_sec * bars * sample_rate)

  output <- numeric(total_samples)

  # Bass rhythm pattern (classic house: root, rest, octave up, rest pattern)
  bass_rhythm <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
                   TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)

  # Build a map of which chord plays in which bar
  bar_to_chord <- integer(bars)
  bar_pos <- 1
  for (i in seq_along(duration_pattern)) {
    chord_bars <- duration_pattern[i]
    for (b in seq_len(chord_bars)) {
      if (bar_pos <= bars) {
        bar_to_chord[bar_pos] <- i
        bar_pos <- bar_pos + 1
      }
    }
  }

  for (bar in seq_len(bars)) {
    # Get chord root for this bar based on duration pattern
    chord_idx <- bar_to_chord[bar]
    if (chord_idx == 0) chord_idx <- 1
    chord_spec <- prog_spec[[(chord_idx - 1) %% length(prog_spec) + 1]]

    scale_idx <- chord_spec$root
    if (scale_idx > length(scale)) scale_idx <- ((scale_idx - 1) %% length(scale)) + 1
    root_midi <- scale[scale_idx]

    # Bass plays 2 octaves below chord root
    bass_root <- root_midi - 24

    bar_offset <- (bar - 1) * as.integer(bar_duration_sec * sample_rate)

    for (step in seq_along(bass_rhythm)) {
      if (bass_rhythm[step]) {
        # Alternate between root and octave up for movement
        note <- if (step %in% c(3, 7, 11, 15)) bass_root + 12 else bass_root

        position <- bar_offset + as.integer((step - 1) * step_duration_sec * sample_rate) + 1

        # Generate bass note
        bass_wave <- raver_bass_note(note, step_duration_sec * 1.5, velocity = 0.85)

        # Mix into output
        wave_samples <- bass_wave@left
        n_wave <- length(wave_samples)
        end_pos <- min(position + n_wave - 1, total_samples)

        if (position <= total_samples && end_pos >= position) {
          n_to_mix <- end_pos - position + 1
          output[position:end_pos] <- output[position:end_pos] + wave_samples[seq_len(n_to_mix)]
        }
      }
    }
  }

  tuneR::Wave(
    left = output,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Generate lead melody for a section
#'
#' @description Internal helper that creates lead melodies from function motifs.
#'   Each function's motif provides a unique rhythm and note pattern derived from
#'   its name. Different sections transform these rhythms in distinct ways.
#'   Uses pentatonic scale for pleasant melodic content.
#'   Includes two lead voices: main melodic lead and percussive pluck with delay.
#'
#' @param arrangement Full arrangement object
#' @param section Section spec
#' @param bars Number of bars
#' @param sample_rate Sample rate
#'
#' @return A Wave object with lead melody, or NULL if no motifs
#' @keywords internal
generate_lead_melody <- function(arrangement, section, bars, sample_rate = SAMPLE_RATE) {
  bpm <- arrangement$bpm
  motifs <- arrangement$motifs
  section_type <- section$type
  key <- arrangement$key

  # Need at least one motif for lead
  if (length(motifs) == 0) {
    return(NULL)
  }

  # Build pentatonic scale for melodic content (sounds good with any notes)
  penta_scale <- raver_build_scale(key, "minor_pentatonic", 2)

  # Calculate timing
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / 16
  total_samples <- as.integer(bar_duration_sec * bars * sample_rate)

  # Dotted 8th delay time (classic house delay)
  dotted_8th_sec <- step_duration_sec * 3  # 3 x 16th = dotted 8th

  # Initialize outputs for both leads
  lead1_output <- numeric(total_samples)
  lead2_output <- numeric(total_samples)

  # Section-specific rhythm transformation and rendering
  for (bar in seq_len(bars)) {
    bar_offset <- (bar - 1) * as.integer(bar_duration_sec * sample_rate)

    # Select which motif to use - cycle through all function motifs
    motif_idx <- ((bar - 1) %% length(motifs)) + 1
    base_motif <- motifs[[motif_idx]]

    # Map motif notes to pentatonic scale
    penta_motif <- map_to_pentatonic(base_motif, penta_scale)

    # Transform rhythm based on section type
    transformed <- transform_motif_for_section(penta_motif, section_type, bar, bars)

    # Create complementary rhythm for lead 2 (offset by 2 steps)
    lead2_rhythm <- c(transformed$rhythm[15:16], transformed$rhythm[1:14])

    # Render lead 1 (melodic, smooth)
    active_steps <- which(transformed$rhythm)
    for (i in seq_along(active_steps)) {
      step <- active_steps[i]
      note_idx <- ((i - 1) %% length(transformed$notes)) + 1
      vel_idx <- ((i - 1) %% length(transformed$velocities)) + 1

      note <- transformed$notes[note_idx] + transformed$octave_shift
      velocity <- transformed$velocities[vel_idx] * transformed$velocity_scale

      note_duration <- step_duration_sec * transformed$note_length

      position <- bar_offset + as.integer((step - 1) * step_duration_sec * sample_rate) + 1
      note_wave <- raver_lead_note(note, note_duration, velocity)

      wave_samples <- note_wave@left
      end_pos <- min(position + length(wave_samples) - 1, total_samples)
      if (position <= total_samples && end_pos >= position) {
        n_mix <- end_pos - position + 1
        lead1_output[position:end_pos] <- lead1_output[position:end_pos] + wave_samples[seq_len(n_mix)]
      }
    }

    # Render lead 2 (percussive pluck, with delay) - only in energetic sections
    if (section_type %in% c("drop", "build")) {
      active_steps2 <- which(lead2_rhythm)
      for (i in seq_along(active_steps2)) {
        step <- active_steps2[i]
        note_idx <- ((i - 1) %% length(transformed$notes)) + 1

        # Lead 2 plays an octave higher
        note <- transformed$notes[note_idx] + transformed$octave_shift + 12
        velocity <- 0.5 * transformed$velocity_scale

        position <- bar_offset + as.integer((step - 1) * step_duration_sec * sample_rate) + 1
        note_wave <- raver_pluck_note(note, step_duration_sec * 1.5, velocity)

        wave_samples <- note_wave@left
        end_pos <- min(position + length(wave_samples) - 1, total_samples)
        if (position <= total_samples && end_pos >= position) {
          n_mix <- end_pos - position + 1
          lead2_output[position:end_pos] <- lead2_output[position:end_pos] + wave_samples[seq_len(n_mix)]
        }
      }
    }
  }

  # Apply rhythmic delay to lead 2
  if (any(lead2_output != 0)) {
    lead2_output <- apply_rhythmic_delay(lead2_output, dotted_8th_sec,
                                          feedback = 0.45, mix = 0.6)
  }

  # Mix both leads
  combined <- lead1_output + lead2_output * 0.6

  tuneR::Wave(left = combined, samp.rate = sample_rate, bit = BIT_DEPTH, pcm = PCM_MODE)
}

#' Map motif notes to pentatonic scale
#'
#' @description Converts motif notes to the nearest notes in a pentatonic scale.
#'   Ensures all melodic content fits within the pleasant pentatonic framework.
#'
#' @param motif Original motif
#' @param penta_scale Pentatonic scale notes (MIDI numbers)
#'
#' @return Motif with notes mapped to pentatonic scale
#' @keywords internal
map_to_pentatonic <- function(motif, penta_scale) {
  # For each note in motif, find nearest pentatonic note
  new_notes <- vapply(motif$notes, function(n) {
    # Find which octave the note is in relative to scale
    octave_offset <- ((n - penta_scale[1]) %/% 12) * 12L
    # Map to scale degree
    note_in_octave <- ((n - penta_scale[1]) %% 12)
    # Find nearest scale degree
    scale_degrees <- (penta_scale - penta_scale[1]) %% 12
    nearest_idx <- which.min(abs(scale_degrees - note_in_octave))
    # Return mapped note as integer
    as.integer(penta_scale[nearest_idx] + octave_offset)
  }, integer(1))

  motif$notes <- new_notes
  motif
}

#' Transform motif rhythm and notes for a specific section type
#'
#' @description Applies section-specific transformations to a motif's rhythm.
#'   Each section type creates a distinct feel while preserving the motif's
#'   identity derived from the function name.
#'
#' @param motif Original motif from generate_motif()
#' @param section_type Section type string
#' @param bar Current bar number
#' @param total_bars Total bars in section
#'
#' @return Transformed motif with rhythm, notes, velocities, and section params
#' @keywords internal
transform_motif_for_section <- function(motif, section_type, bar, total_bars) {
  rhythm <- motif$rhythm
  notes <- motif$notes
  velocities <- motif$velocities
  progress <- bar / total_bars

  result <- switch(section_type,

    # INTRO: Only play downbeats from the motif rhythm, very sparse
    "intro" = {
      # Keep only steps 1, 5, 9, 13 (quarter notes) that are in original rhythm
      sparse_rhythm <- rhythm & (seq_along(rhythm) %in% c(1, 5, 9, 13))
      # If nothing left, just play beat 1
      if (!any(sparse_rhythm)) sparse_rhythm[1] <- TRUE
      list(
        rhythm = sparse_rhythm,
        notes = notes,
        velocities = velocities,
        octave_shift = 12,  # High, ethereal
        velocity_scale = 0.35 + progress * 0.15,
        note_length = 4  # Long notes
      )
    },

    # BUILD: Progressively reveal more of the original rhythm
    "build" = {
      active_indices <- which(rhythm)
      # Reveal more notes as we progress through the section
      n_reveal <- max(1, ceiling(length(active_indices) * progress))
      revealed_rhythm <- rep(FALSE, 16)
      if (length(active_indices) > 0) {
        revealed_rhythm[active_indices[seq_len(n_reveal)]] <- TRUE
      }
      list(
        rhythm = revealed_rhythm,
        notes = notes,
        velocities = velocities,
        octave_shift = 0,
        velocity_scale = 0.5 + progress * 0.3,
        note_length = 2
      )
    },

    # DROP: Full motif rhythm with call-and-response (original + shifted variation)
    "drop" = {
      phrase_pos <- ((bar - 1) %% 4) + 1  # 4-bar phrases

      if (phrase_pos <= 2) {
        # Bars 1-2: Original rhythm
        list(
          rhythm = rhythm,
          notes = notes,
          velocities = velocities,
          octave_shift = 0,
          velocity_scale = 0.7,
          note_length = 2
        )
      } else {
        # Bars 3-4: Inverted/shifted rhythm as response
        shifted_rhythm <- c(rhythm[9:16], rhythm[1:8])  # Shift by half bar
        # Also shift notes for melodic variation
        shifted_notes <- if (length(notes) > 1) c(notes[-1], notes[1] + 5) else notes + 5
        list(
          rhythm = shifted_rhythm,
          notes = shifted_notes,
          velocities = velocities,
          octave_shift = 5,  # Fourth up for response
          velocity_scale = 0.65,
          note_length = 2
        )
      }
    },

    # BREAKDOWN: Half-time feel - stretch rhythm across 2 beats
    "breakdown" = {
      # Convert 16th note rhythm to 8th note (half-time)
      # Only use every other active step
      active_indices <- which(rhythm)
      halftime_rhythm <- rep(FALSE, 16)
      if (length(active_indices) > 0) {
        # Take every other note and place on beats
        sparse_indices <- active_indices[seq(1, length(active_indices), by = 2)]
        # Map to beat positions (1, 5, 9, 13)
        beat_positions <- c(1, 5, 9, 13)
        for (i in seq_along(sparse_indices)) {
          if (i <= length(beat_positions)) {
            halftime_rhythm[beat_positions[i]] <- TRUE
          }
        }
      }
      if (!any(halftime_rhythm)) halftime_rhythm[1] <- TRUE
      list(
        rhythm = halftime_rhythm,
        notes = notes,
        velocities = velocities,
        octave_shift = 12,  # High, atmospheric
        velocity_scale = 0.5,
        note_length = 6  # Long, sustained
      )
    },

    # OUTRO: Decaying fragments of the rhythm
    "outro" = {
      # Randomly drop notes, more as we progress
      keep_prob <- 1 - progress * 0.8
      decayed_rhythm <- rhythm & (runif(16) < keep_prob)
      # Ensure at least one note in early bars
      if (!any(decayed_rhythm) && progress < 0.8) {
        decayed_rhythm[which(rhythm)[1]] <- TRUE
      }
      list(
        rhythm = decayed_rhythm,
        notes = notes,
        velocities = velocities,
        octave_shift = 12,
        velocity_scale = 0.4 * (1 - progress * 0.6),
        note_length = 3
      )
    },

    # DEFAULT: Use original rhythm
    list(
      rhythm = rhythm,
      notes = notes,
      velocities = velocities,
      octave_shift = 0,
      velocity_scale = 0.6,
      note_length = 2
    )
  )

  result
}

#' Reset RNG state
#'
#' @description Resets R's random state after composition to prevent
#'   composition from affecting user's random state.
#'
#' @keywords internal
reset_rng_state <- function() {
  # Generate a random value to advance the RNG
  # This is less disruptive than fully resetting
  invisible(runif(1))
}
