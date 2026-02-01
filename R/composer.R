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
#' @description Validates that BPM is within the acceptable 118-124 range.
#'
#' @param bpm Numeric BPM value to validate
#'
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_bpm <- function(bpm) {
  if (!is.numeric(bpm) || length(bpm) != 1) {
    stop("BPM must be a single numeric value", call. = FALSE)
  }

  if (bpm < 118 || bpm > 124) {
    stop(
      "BPM must be between 118 and 124 for deep house. Got: ", bpm,
      "\n  Tip: Use bpm = NULL for auto-selection within genre range.",
      call. = FALSE
    )
  }

  TRUE
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
raver_compose <- function(code_model, bpm = NULL, seed = NULL, sample_rate = SAMPLE_RATE) {
  # Validate input
  if (!inherits(code_model, "CodeModel")) {
    stop("code_model must be a CodeModel object", call. = FALSE)
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

  # Auto-select BPM if not provided
  if (is.null(bpm)) {
    bpm <- select_bpm(code_model)
  }
  validate_bpm(bpm)

  # Create arrangement
  arrangement <- create_arrangement(code_model, bpm)

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
#' @description Internal helper that creates lead melodies using function motifs.
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

  # Need at least one motif for lead
 if (length(motifs) == 0) {
    return(NULL)
  }

  # Use first motif for main lead
  main_motif <- motifs[[1]]

  # Generate lead melody
  lead_wave <- raver_lead_melody(main_motif, bpm, bars, velocity = 0.6)

  # If multiple motifs, layer a second lead (quieter, evolved)
  if (length(motifs) >= 2) {
    second_motif <- evolve_motif(motifs[[2]], variation = 0.2)
    second_lead <- raver_lead_melody(second_motif, bpm, bars, velocity = 0.35)

    # Mix leads
    lead_samples <- lead_wave@left
    second_samples <- second_lead@left
    n_mix <- min(length(lead_samples), length(second_samples))

    lead_samples[seq_len(n_mix)] <- lead_samples[seq_len(n_mix)] + second_samples[seq_len(n_mix)]

    lead_wave <- tuneR::Wave(
      left = lead_samples,
      samp.rate = sample_rate,
      bit = BIT_DEPTH,
      pcm = PCM_MODE
    )
  }

  lead_wave
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
