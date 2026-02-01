#' @title Code-to-Music Mapping
#' @description Map code features to musical parameters using deterministic
#'   hash-based assignment. Ensures same script always produces same mapping.
#' @name mapping
NULL

#' Map Code to Music Parameters
#'
#' Create deterministic mapping from code features to musical parameters.
#' Uses MD5 hashing to ensure same input always produces same output.
#' Functions map to parts (1-4), variables map to instruments (1-5).
#'
#' @param code_model A CodeModel object from raver_analyze()
#' @return List with function_parts, variable_instruments, and arrangement
#' @export
#'
#' @examples
#' \dontrun{
#' model <- raver_analyze("script.R")
#' mapping <- raver_map_to_music(model)
#' mapping$function_parts        # Named list: function -> part (1-4)
#' mapping$variable_instruments  # Named list: variable -> instrument (1-5)
#' mapping$arrangement           # Overall arrangement parameters
#' }
raver_map_to_music <- function(code_model) {
  # Validate input

  if (!inherits(code_model, "CodeModel")) {
    stop("code_model must be a CodeModel object", call. = FALSE)
  }

  # Map functions to parts (1-4) using deterministic hashing
  function_parts <- map_functions_to_parts(code_model$functions)

  # Map variables to instruments (1-5) using deterministic hashing
  variable_instruments <- map_variables_to_instruments(code_model$variables)

  # Calculate arrangement parameters
  arrangement <- calculate_arrangement(code_model)

  list(
    function_parts = function_parts,
    variable_instruments = variable_instruments,
    arrangement = arrangement
  )
}

#' Map Functions to Parts
#'
#' Hash each function name to determine part assignment (1-4).
#' Uses MD5 hash for determinism.
#'
#' @param functions Character vector of function names
#' @return Named list mapping function names to parts (1-4)
#' @keywords internal
map_functions_to_parts <- function(functions) {
  if (length(functions) == 0) {
    return(structure(list(), names = character(0)))
  }

  parts <- lapply(functions, function(fname) {
    # Get MD5 hash, take first 7 hex chars, convert to integer
    hash_str <- digest::digest(fname, algo = "md5")
    hash_int <- strtoi(substr(hash_str, 1, 7), base = 16L)
    # Map to part 1-4
    (hash_int %% 4L) + 1L
  })

  names(parts) <- functions
  parts
}

#' Map Variables to Instruments
#'
#' Hash each variable name to determine instrument assignment (1-5).
#' Uses MD5 hash for determinism.
#'
#' @param variables Character vector of variable names
#' @return Named list mapping variable names to instruments (1-5)
#' @keywords internal
map_variables_to_instruments <- function(variables) {
  if (length(variables) == 0) {
    return(structure(list(), names = character(0)))
  }

  instruments <- lapply(variables, function(vname) {
    # Get MD5 hash, take first 7 hex chars, convert to integer
    hash_str <- digest::digest(vname, algo = "md5")
    hash_int <- strtoi(substr(hash_str, 1, 7), base = 16L)
    # Map to instrument 1-5
    (hash_int %% 5L) + 1L
  })

  names(instruments) <- variables
  instruments
}

#' Calculate Arrangement Parameters
#'
#' Derive overall arrangement parameters from CodeModel.
#' Part count and instrument count come from CodeModel.
#' Density comes from density_level.
#' Drum intensity is derived from complexity metrics.
#'
#' @param code_model A CodeModel object
#' @return List with part_count, instrument_count, density, drum_intensity
#' @keywords internal
calculate_arrangement <- function(code_model) {
  # Drum intensity: based on nesting depth and control flow
  # More complex code = more intense drums
  raw_intensity <- (code_model$nesting_depth / 5) +
    (code_model$control_flow_count / 20)
  drum_intensity <- min(raw_intensity, 1.0)

  list(
    part_count = code_model$part_count,
    instrument_count = code_model$instrument_count,
    density = code_model$density_level,
    drum_intensity = drum_intensity
  )
}
