#' @title Track Arrangement System
#' @description Track structure and section arrangement for deep house tracks.
#'   Maps code complexity to section structure and manages the progression
#'   from intro through build, drop, breakdown, and outro.
#' @name arrangement
NULL

# =============================================================================
# Section Type Definitions
# =============================================================================

#' Section type definitions for deep house tracks
#'
#' @description Named list defining the characteristics of each track section.
#'   Each section has bars, description, elements, and filter envelope settings.
#'
#' @details
#' Section types:
#' - intro: Minimal elements, building anticipation (8 bars)
#' - build: Adding elements, filter sweep up (8 bars)
#' - drop: Full energy, all elements including lead (16 bars)
#' - breakdown: Stripped back, pads and lead prominent (8 bars)
#' - outro: Elements dropping out (8 bars)
#'
#' @export
SECTION_TYPES <- list(
  intro = list(
    bars = 8L,
    description = "Minimal elements, building anticipation",
    elements = c("kick", "hihat_closed"),
    filter_start = 200,
    filter_end = 800
  ),
  build = list(
    bars = 8L,
    description = "Adding elements, filter sweep up",
    elements = c("kick", "hihat_closed", "snare", "bass", "lead"),
    filter_start = 800,
    filter_end = 4000
  ),
  drop = list(
    bars = 16L,
    description = "Full energy, all elements",
    elements = c("kick", "hihat_closed", "snare", "clap", "bass", "pad", "lead"),
    filter_start = 4000,
    filter_end = 4000
  ),
  breakdown = list(
    bars = 8L,
    description = "Stripped back, pads and lead prominent",
    elements = c("pad", "hihat_closed", "lead"),
    filter_start = 2000,
    filter_end = 1000
  ),
  outro = list(
    bars = 8L,
    description = "Elements dropping out",
    elements = c("kick", "hihat_closed"),
    filter_start = 1000,
    filter_end = 200
  )
)

# =============================================================================
# Arrangement Creation
# =============================================================================

#' Create a track arrangement from code model
#'
#' @description Maps code complexity to track structure. More functions lead
#'   to more sections, higher complexity leads to longer sections.
#'   Supports dynamic arrangement changes when complexity changes.
#'
#' @param code_model A CodeModel object from raver_analyze()
#' @param bpm Numeric beats per minute. Default 120.
#' @param genre_config List from get_genre_config() for genre-specific arrangement
#' @param previous_model Optional previous CodeModel for detecting complexity changes
#'
#' @return List with:
#'   \itemize{
#'     \item bpm: Tempo in beats per minute
#'     \item key: MIDI root note for the track
#'     \item sections: List of section specs
#'     \item complexity_change: Direction of complexity change ("up", "down", "same")
#'     \item total_bars: Total bars in arrangement
#'     \item motifs: Generated motifs for each function
#'   }
#'
#' @details
#' The arrangement maps code characteristics to musical structure:
#' - Number of functions determines section count and complexity
#' - Nesting depth and control flow affect section lengths
#' - Each function gets a unique motif via generate_motif()
#'
#' @examples
#' \dontrun{
#' model <- raver_analyze("script.R")
#' arr <- create_arrangement(model, bpm = 122)
#' arr$sections  # List of section specs
#' arr$motifs    # Function-specific motifs
#' }
#'
#' @export
create_arrangement <- function(code_model, bpm = 120, genre_config = NULL, previous_model = NULL) {
  # Validate input
  if (!inherits(code_model, "CodeModel")) {
    stop("code_model must be a CodeModel object", call. = FALSE)
  }

  # Determine key from file path
  key <- hash_to_key(code_model$file_path)

  # Build scale for motif generation
  scale_notes <- raver_build_scale(key, "natural_minor", 2)

  # Generate motifs for each function
  functions <- code_model$functions
  motifs <- lapply(functions, function(fname) {
    generate_motif(fname, scale_notes)
  })
  names(motifs) <- functions

  # Calculate complexity factor (0-1)
  complexity <- calculate_complexity_factor(code_model)

  # Detect complexity change for dynamic arrangement
  complexity_change <- "same"
  if (!is.null(previous_model)) {
    prev_complexity <- calculate_complexity_factor(previous_model)
    if (complexity > prev_complexity + 0.1) {
      complexity_change <- "up"
    } else if (complexity < prev_complexity - 0.1) {
      complexity_change <- "down"
    }
  }

  # Build section structure based on complexity and change direction
  sections <- build_sections(complexity, code_model, complexity_change, genre_config)

  # Calculate total bars
  total_bars <- sum(vapply(sections, function(s) s$bars, integer(1)))

  list(
    bpm = bpm,
    key = key,
    sections = sections,
    total_bars = total_bars,
    motifs = motifs,
    complexity_change = complexity_change
  )
}

#' Calculate complexity factor from code model
#'
#' @description Internal helper to derive a 0-1 complexity factor from
#'   CodeModel metrics.
#'
#' @param code_model A CodeModel object
#'
#' @return Numeric complexity factor from 0.0 to 1.0
#' @keywords internal
calculate_complexity_factor <- function(code_model) {
  # Combine multiple metrics into complexity score
  # Normalize each to roughly 0-1 range
  func_score <- min(length(code_model$functions) / 10, 1.0)
  nest_score <- min(code_model$nesting_depth / 5, 1.0)
  flow_score <- min(code_model$control_flow_count / 20, 1.0)
  cyclo_score <- min(code_model$cyclomatic_complexity / 15, 1.0)

  # Weighted average
  complexity <- 0.3 * func_score + 0.25 * nest_score +
                0.25 * flow_score + 0.2 * cyclo_score

  max(0.0, min(1.0, complexity))
}

#' Build section structure based on complexity
#'
#' @description Internal helper to create the section list based on
#'   code complexity.
#'
#' @param complexity Numeric complexity factor (0-1)
#' @param code_model A CodeModel object
#' @param complexity_change Direction of complexity change ("up", "down", "same")
#' @param genre_config Genre configuration list (optional)
#'
#' @return List of section specs
#' @keywords internal
build_sections <- function(complexity, code_model, complexity_change = "same", genre_config = NULL) {
  # Extended structure for longer, more dynamic tracks
  # Structure adapts based on complexity change direction
  sections <- list()

  # Dynamic adjustment factors based on complexity change
  change_multiplier <- switch(complexity_change,
    "up" = 1.3,      # Complexity increased = longer sections
    "down" = 0.7,    # Complexity decreased = shorter, more breakdowns
    "same" = 1.0     # No change = normal
  )

  # Intro - atmospheric opening (shorter if complexity dropped)
  intro_bars <- as.integer(
    (if (complexity > 0.5) 16L else 8L) *
    ifelse(complexity_change == "down", 0.5, 1.0)
  )
  sections$intro <- list(
    type = "intro",
    bars = intro_bars,
    description = SECTION_TYPES$intro$description,
    elements = SECTION_TYPES$intro$elements,
    filter_start = SECTION_TYPES$intro$filter_start,
    filter_end = SECTION_TYPES$intro$filter_end
  )

  # First Build
  build_bars <- as.integer(if (complexity > 0.6) 16L else 8L * change_multiplier)
  sections$build <- list(
    type = "build",
    bars = build_bars,
    description = SECTION_TYPES$build$description,
    elements = SECTION_TYPES$build$elements,
    filter_start = SECTION_TYPES$build$filter_start,
    filter_end = SECTION_TYPES$build$filter_end
  )

  # First Drop - main energy (longer if complexity increased)
  drop_bars <- as.integer(
    (if (complexity > 0.7) 32L else if (complexity > 0.4) 24L else 16L) *
    change_multiplier
  )
  sections$drop <- list(
    type = "drop",
    bars = drop_bars,
    description = SECTION_TYPES$drop$description,
    elements = SECTION_TYPES$drop$elements,
    filter_start = SECTION_TYPES$drop$filter_start,
    filter_end = SECTION_TYPES$drop$filter_end
  )

  # First Breakdown - tension release (longer if complexity dropped)
  breakdown_bars <- as.integer(
    (if (complexity > 0.7) 16L else 8L) *
    ifelse(complexity_change == "down", 1.5, 1.0)
  )
  sections$breakdown <- list(
    type = "breakdown",
    bars = breakdown_bars,
    description = SECTION_TYPES$breakdown$description,
    elements = SECTION_TYPES$breakdown$elements,
    filter_start = SECTION_TYPES$breakdown$filter_start,
    filter_end = SECTION_TYPES$breakdown$filter_end
  )

  # Second Build - rebuilding energy (shorter if complexity dropped)
  sections$build2 <- list(
    type = "build",
    bars = as.integer(build_bars * ifelse(complexity_change == "down", 0.8, 1.0)),
    description = "Rebuilding energy after breakdown",
    elements = SECTION_TYPES$build$elements,
    filter_start = 600,
    filter_end = 4000
  )

  # Second Drop - peak energy
  sections$drop2 <- list(
    type = "drop",
    bars = drop_bars,
    description = "Peak energy section",
    elements = SECTION_TYPES$drop$elements,
    filter_start = SECTION_TYPES$drop$filter_start,
    filter_end = SECTION_TYPES$drop$filter_end
  )

  # Extra breakdown/drop for complex tracks or complexity changes
  if (complexity > 0.4 || complexity_change != "same") {
    sections$breakdown2 <- list(
      type = "breakdown",
      bars = as.integer(breakdown_bars %/% 2L),
      description = "Brief tension break",
      elements = SECTION_TYPES$breakdown$elements,
      filter_start = 2000,
      filter_end = 1500
    )

    # Third Drop - final climax (only if complexity increased)
    if (complexity_change == "up") {
      sections$drop3 <- list(
        type = "drop",
        bars = drop_bars,
        description = "Final climax",
        elements = SECTION_TYPES$drop$elements,
        filter_start = SECTION_TYPES$drop$filter_start,
        filter_end = SECTION_TYPES$drop$filter_end
      )
    }
  }

  # Outro - wind down
  outro_bars <- as.integer(if (complexity > 0.5) 16L else 8L)
  sections$outro <- list(
    type = "outro",
    bars = outro_bars,
    description = SECTION_TYPES$outro$description,
    elements = SECTION_TYPES$outro$elements,
    filter_start = SECTION_TYPES$outro$filter_start,
    filter_end = SECTION_TYPES$outro$filter_end
  )

  sections
}

# =============================================================================
# Section Rendering
# =============================================================================

#' Render a single section to audio
#'
#' @description Renders a section to a Wave object. Creates patterns for each
#'   element and applies filter envelope.
#'
#' @param section A section spec from arrangement$sections
#' @param arrangement The full arrangement object
#' @param sample_rate Integer sample rate. Default SAMPLE_RATE.
#'
#' @return A Wave object containing the rendered section
#'
#' @details
#' The section is rendered by:
#' 1. Creating patterns for drums only
#' 2. Generating bass line following chord progression
#' 3. Generating pad progression (with chord changes)
#' 4. Generating lead melody from function motifs
#' 5. Mixing all elements
#' 6. Applying filter envelope
#'
#' @examples
#' \dontrun{
#' model <- raver_analyze("script.R")
#' arr <- create_arrangement(model)
#' intro_wave <- render_section(arr$sections$intro, arr)
#' }
#'
#' @export
render_section <- function(section, arrangement, sample_rate = SAMPLE_RATE) {
  bpm <- arrangement$bpm
  bars <- section$bars
  elements <- section$elements

  # Calculate bar duration
  bar_duration_sec <- (60 / bpm) * 4
  bar_samples <- as.integer(bar_duration_sec * sample_rate)
  total_samples <- bar_samples * bars

  # Separate melodic elements - they get special progression/motif handling
  has_pad <- "pad" %in% elements
  has_bass <- "bass" %in% elements
  has_lead <- "lead" %in% elements
  drum_elements <- setdiff(elements, c("pad", "bass", "lead"))

  # Create patterns for drums only
  patterns <- create_section_patterns(drum_elements, arrangement)

  # Determine fill type based on section
  fill_type <- switch(section$type,
    "intro" = "buildup",
    "build" = "buildup",
    "drop" = "basic",
    "breakdown" = "breakdown",
    "outro" = "breakdown",
    "basic"
  )

  # Render all bars for drums
  bar_waves <- list()
  for (bar in seq_len(bars)) {
    bar_wave <- combine_patterns(patterns, bpm, sample_rate)

    # Add drum fill every 8 bars (on bars 8, 16, 24, 32, etc.) for variation
    # Also always add on the last bar of the section
    # 80% chance to play fill for natural variation
    is_fill_bar <- (bar %% 8 == 0) || (bar == bars)
    should_play_fill <- runif(1) < 0.80

    if (is_fill_bar && should_play_fill && length(drum_elements) > 0) {
      fill_wave <- generate_drum_fill(fill_type, bpm, sample_rate)
      fill_samples <- fill_wave@left
      bar_samples_vec <- bar_wave@left
      fill_level <- 0.75

      n_mix <- min(length(fill_samples), length(bar_samples_vec))
      bar_samples_vec[seq_len(n_mix)] <- bar_samples_vec[seq_len(n_mix)] +
                                          fill_samples[seq_len(n_mix)] * fill_level

      bar_wave <- tuneR::Wave(
        left = bar_samples_vec,
        samp.rate = sample_rate,
        bit = BIT_DEPTH,
        pcm = PCM_MODE
      )
    }

    bar_waves[[bar]] <- bar_wave
  }

  # Concatenate drum bars
  all_samples <- numeric(total_samples)
  for (i in seq_along(bar_waves)) {
    start_pos <- (i - 1) * bar_samples + 1
    end_pos <- min(i * bar_samples, total_samples)
    wave_samples <- bar_waves[[i]]@left
    n_to_copy <- min(length(wave_samples), end_pos - start_pos + 1)
    all_samples[start_pos:(start_pos + n_to_copy - 1)] <- wave_samples[seq_len(n_to_copy)]
  }

  # Generate and mix bass line following chord progression
  if (has_bass) {
    bass_wave <- generate_bass_line(arrangement, section, bars, sample_rate)
    bass_samples <- bass_wave@left
    bass_level <- 0.75
    n_to_mix <- min(length(bass_samples), length(all_samples))
    all_samples[seq_len(n_to_mix)] <- all_samples[seq_len(n_to_mix)] +
                                       bass_samples[seq_len(n_to_mix)] * bass_level
  }

  # Generate and mix pad progression
  if (has_pad) {
    pad_wave <- generate_pad_progression(arrangement, section, bars, sample_rate)
    pad_samples <- pad_wave@left
    pad_level <- 0.70
    n_to_mix <- min(length(pad_samples), length(all_samples))
    all_samples[seq_len(n_to_mix)] <- all_samples[seq_len(n_to_mix)] +
                                       pad_samples[seq_len(n_to_mix)] * pad_level
  }

  # Generate and mix lead melody from motifs
  if (has_lead && length(arrangement$motifs) > 0) {
    lead_wave <- generate_lead_melody(arrangement, section, bars, sample_rate)
    if (!is.null(lead_wave)) {
      lead_samples <- lead_wave@left
      lead_level <- 0.5
      n_to_mix <- min(length(lead_samples), length(all_samples))
      all_samples[seq_len(n_to_mix)] <- all_samples[seq_len(n_to_mix)] +
                                         lead_samples[seq_len(n_to_mix)] * lead_level
    }
  }

  # Apply filter envelope
  filtered <- apply_section_filter(all_samples, section$filter_start,
                                   section$filter_end, bars, sample_rate)

  tuneR::Wave(
    left = filtered,
    samp.rate = sample_rate,
    bit = BIT_DEPTH,
    pcm = PCM_MODE
  )
}

#' Generate a drum fill pattern for transitions
#'
#' @description Creates a drum fill pattern for the last bar of a section.
#'   Uses snare rolls building to the downbeat.
#'
#' @param fill_type Character type of fill: "basic", "buildup", "breakdown"
#' @param bpm Numeric beats per minute
#' @param sample_rate Integer sample rate
#'
#' @return A Wave object containing the drum fill
#' @keywords internal
generate_drum_fill <- function(fill_type = "basic", bpm = 120, sample_rate = SAMPLE_RATE) {
  bar_duration_sec <- (60 / bpm) * 4
  step_duration_sec <- bar_duration_sec / 16
  total_samples <- as.integer(bar_duration_sec * sample_rate)

  output <- numeric(total_samples)

  # Different fill patterns based on type
  if (fill_type == "buildup") {
    # Building snare roll: sparse to dense in second half of bar
    # Steps 9-16 get progressively denser
    snare_steps <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                     TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
    velocities <- c(0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0.6, 0, 0.7, 0.8, 0.9, 1.0)
  } else if (fill_type == "breakdown") {
    # Minimal fill - just accent hits
    snare_steps <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                     FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
    velocities <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0, 0.8, 0)
  } else {
    # Basic fill - classic house fill pattern
    snare_steps <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                     TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
    velocities <- c(0, 0, 0, 0, 0, 0, 0, 0, 0.6, 0, 0.7, 0, 0.8, 0, 0.9, 1.0)
  }

  # Render snare hits
  for (step in seq_along(snare_steps)) {
    if (snare_steps[step]) {
      position <- as.integer((step - 1) * step_duration_sec * sample_rate) + 1
      velocity <- velocities[step]

      # Load and scale snare sample
      snare_wave <- raver_drum_hit("snare", velocity)
      wave_samples <- snare_wave@left

      n_wave <- length(wave_samples)
      end_pos <- min(position + n_wave - 1, total_samples)

      if (position <= total_samples && end_pos >= position) {
        n_to_mix <- end_pos - position + 1
        output[position:end_pos] <- output[position:end_pos] + wave_samples[seq_len(n_to_mix)]
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

#' Create patterns for section elements
#'
#' @description Internal helper to create patterns for drum elements in a section.
#'   Melodic elements (bass, pad, lead) are handled separately with progression/motif systems.
#'
#' @param elements Character vector of element names (should be drums only)
#' @param arrangement The arrangement object
#'
#' @return List of patterns
#' @keywords internal
create_section_patterns <- function(elements, arrangement) {
  patterns <- list()

  # Standard deep house drum patterns
  kick_steps <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                  TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

  snare_steps <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                   FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

  clap_steps <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                  FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

  hihat_steps <- rep(TRUE, 16)

  hihat_open_steps <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
                        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)

  for (elem in elements) {
    pattern <- switch(elem,
      kick = create_pattern("kick", kick_steps),
      snare = create_pattern("snare", snare_steps),
      clap = create_pattern("clap", clap_steps, velocity = 0.85),
      hihat_closed = create_pattern("hihat_closed", hihat_steps, swing = 0.08, velocity = 0.35),
      hihat_open = create_pattern("hihat_open", hihat_open_steps, velocity = 0.4),
      NULL
    )

    if (!is.null(pattern)) {
      patterns[[length(patterns) + 1]] <- pattern
    }
  }

  patterns
}

#' Apply filter envelope to section audio
#'
#' @description Applies a lowpass filter sweep across the section, interpolating
#'   from start_hz to end_hz over the duration.
#'
#' @param samples Numeric vector of audio samples
#' @param start_hz Numeric starting filter cutoff in Hz
#' @param end_hz Numeric ending filter cutoff in Hz
#' @param bars Integer number of bars in section
#' @param sample_rate Integer sample rate
#'
#' @return Numeric vector of filtered samples
#'
#' @export
apply_section_filter <- function(samples, start_hz, end_hz, bars, sample_rate = SAMPLE_RATE) {
  n_samples <- length(samples)
  bar_samples <- n_samples / bars

  output <- numeric(n_samples)

  for (bar in seq_len(bars)) {
    # Calculate filter cutoff for this bar (linear interpolation)
    progress <- (bar - 0.5) / bars  # Center of bar
    cutoff_hz <- start_hz + (end_hz - start_hz) * progress

    # Get bar range
    start_idx <- as.integer((bar - 1) * bar_samples) + 1
    end_idx <- as.integer(bar * bar_samples)
    end_idx <- min(end_idx, n_samples)

    bar_audio <- samples[start_idx:end_idx]

    # Apply one-pole lowpass filter
    # alpha = dt / (RC + dt), where RC = 1 / (2 * pi * cutoff)
    dt <- 1 / sample_rate
    rc <- 1 / (2 * pi * cutoff_hz)
    alpha <- dt / (rc + dt)

    filtered_bar <- numeric(length(bar_audio))
    filtered_bar[1] <- bar_audio[1]
    for (i in 2:length(bar_audio)) {
      filtered_bar[i] <- filtered_bar[i - 1] + alpha * (bar_audio[i] - filtered_bar[i - 1])
    }

    output[start_idx:end_idx] <- filtered_bar
  }

  output
}

# =============================================================================
# Element Selection
# =============================================================================

#' Get section elements based on density
#'
#' @description Returns the list of elements for a section type, optionally
#'   reduced based on density.
#'
#' @param section_type Character section type name (e.g., "drop", "intro")
#' @param density Numeric density from 0.0 to 1.0. Lower values reduce
#'   the number of elements. Default 1.0.
#'
#' @return Character vector of element names
#'
#' @examples
#' \dontrun{
#' elements_full <- get_section_elements("drop", 1.0)
#' elements_sparse <- get_section_elements("drop", 0.3)
#' }
#'
#' @export
get_section_elements <- function(section_type, density = 1.0) {
  if (!(section_type %in% names(SECTION_TYPES))) {
    warning("Unknown section type: ", section_type, ". Using 'drop'.")
    section_type <- "drop"
  }

  elements <- SECTION_TYPES[[section_type]]$elements

  # Reduce elements based on density
  density <- max(0.0, min(1.0, density))

  if (density >= 1.0) {
    return(elements)
  }

  # Calculate how many elements to keep
  n_elements <- length(elements)
  n_keep <- max(1L, as.integer(ceiling(n_elements * density)))

  # Priority order: kick and hihat are core, then others
  priority <- c("kick", "hihat_closed", "bass", "snare", "clap", "pad")
  ordered <- intersect(priority, elements)
  remaining <- setdiff(elements, ordered)
  ordered <- c(ordered, remaining)

  ordered[seq_len(n_keep)]
}
