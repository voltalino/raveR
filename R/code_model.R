#' @title CodeModel R6 Class
#' @description R6 class for immutable representation of analyzed R code.
#'   Stores extracted features (functions, variables, calls) and computed
#'   musical parameters for audio generation.
#' @name code_model
NULL

#' CodeModel Class
#'
#' An R6 class that stores the structural analysis of an R script.
#' Contains extracted features (functions, variables, calls), file metadata
#' (path, hash, line count), and computed musical parameters (part count,
#' instrument count, density level).
#'
#' @export
CodeModel <- R6::R6Class(
  "CodeModel",

  public = list(
    #' @field file_path Path to the analyzed R script
    file_path = NULL,

    #' @field file_hash MD5 hash of file contents for change detection
    file_hash = NULL,

    #' @field functions Character vector of function names defined in script
    functions = character(),

    #' @field function_calls Character vector of functions called in script
    function_calls = character(),

    #' @field variables Character vector of variables assigned in script
    variables = character(),

    #' @field cyclomatic_complexity Cyclomatic complexity metric (set by Plan 02)
    cyclomatic_complexity = 0L,

    #' @field nesting_depth Maximum nesting depth (set by Plan 02)
    nesting_depth = 0L,

    #' @field control_flow_count Count of control flow statements (set by Plan 02)
    control_flow_count = 0L,

    #' @field line_count Number of lines in source file
    line_count = 0L,

    #' @field part_count Musical parameter: number of parts (1-4)
    part_count = 1L,

    #' @field instrument_count Musical parameter: number of instruments (1-5)
    instrument_count = 1L,

    #' @field density_level Musical parameter: density (0-1)
    density_level = 0,

    #' Initialize CodeModel
    #'
    #' @param file_path Path to the analyzed R script
    #' @param features List with function_definitions, function_calls, variable_assignments
    #' @param metrics List with cyclomatic, nesting_depth, control_flow_count
    #' @param minimal_features Optional fallback features from extract_minimal_features()
    initialize = function(file_path, features, metrics, minimal_features = NULL) {
      self$file_path <- file_path
      self$file_hash <- digest::digest(file = file_path)

      # Handle edge case: empty or minimal files
      if (!is.null(minimal_features) && minimal_features$is_minimal) {
        # Use minimal fallback features for broken/unparseable files
        self$functions <- paste0("func_", seq_len(max(minimal_features$function_hints, 1)))
        self$function_calls <- character()
        self$variables <- paste0("var_", seq_len(max(minimal_features$line_count %/% 5, 1)))
        self$cyclomatic_complexity <- as.integer(max(minimal_features$bracket_depth, 1))
        self$nesting_depth <- as.integer(max(minimal_features$bracket_depth, 1))
        self$control_flow_count <- as.integer(minimal_features$comment_lines)
        self$line_count <- as.integer(minimal_features$line_count)
      } else {
        # Normal feature extraction
        self$functions <- features$function_definitions
        self$function_calls <- features$function_calls
        self$variables <- features$variable_assignments
        self$cyclomatic_complexity <- as.integer(metrics$cyclomatic)
        self$nesting_depth <- as.integer(metrics$nesting_depth)
        self$control_flow_count <- as.integer(metrics$control_flow_count)
        self$line_count <- length(readLines(file_path, warn = FALSE))
      }

      # Ensure minimum viable values for empty scripts
      if (length(self$functions) == 0) self$functions <- "main"
      if (length(self$variables) == 0) self$variables <- "x"

      private$calculate_musical_params()

      invisible(self)
    },

    #' Check if Same File
    #'
    #' Compare file hashes to detect if the underlying file has changed.
    #' Used for file change detection in live mode.
    #'
    #' @param other Another CodeModel object to compare against
    #' @return TRUE if file hashes match, FALSE otherwise
    is_same_as = function(other) {
      identical(self$file_hash, other$file_hash)
    },

    #' Summary String
    #'
    #' Returns a formatted string with key statistics.
    #'
    #' @return Character string with summary
    summary = function() {
      sprintf(
        "CodeModel: %s | %d functions, %d variables | complexity: %d",
        basename(self$file_path),
        length(self$functions),
        length(self$variables),
        self$cyclomatic_complexity
      )
    }
  ),

  private = list(
    #' Calculate Musical Parameters
    #'
    #' Compute part_count, instrument_count, and density_level from
    #' extracted features and metrics.
    calculate_musical_params = function() {
      # Part count: based on function count (1-4 range)
      # More functions = more musical parts
      self$part_count <- as.integer(
        min(ceiling((length(self$functions) + 1) / 2), 4L)
      )

      # Instrument count: based on variable count (1-5 range)
      # More variables = more instruments
      self$instrument_count <- as.integer(
        min(max(length(self$variables), 1L), 5L)
      )

      # Density level: based on complexity (0-1 range)
      # Higher complexity = denser arrangement
      self$density_level <- min(self$cyclomatic_complexity / 20, 1.0)
    }
  )
)
