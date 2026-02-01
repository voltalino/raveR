#' @title R Script Analysis
#' @description Parse R scripts to extract structural information including
#'   function definitions, function calls, and variable assignments.
#' @name analysis
NULL

#' Safe Parse R Script
#'
#' Internal helper that wraps parse() in tryCatch for graceful error handling.
#' Returns empty expression on parse error instead of crashing.
#'
#' @param file_path Path to R script file
#' @return Parsed expression, or expression() on error
#' @keywords internal
safe_parse <- function(file_path) {
  tryCatch(
    parse(file = file_path, keep.source = TRUE),
    error = function(e) {
      warning(
        "Parse error in '", basename(file_path), "': ", conditionMessage(e),
        call. = FALSE
      )
      expression()
    }
  )
}

#' Extract Features from Parse Data
#'
#' Internal helper that extracts function definitions, function calls,
#' and variable assignments from parse data.
#'
#' @param pd Parse data frame from getParseData()
#' @return List with function_definitions, function_calls, variable_assignments
#' @keywords internal
#' @export
extract_features <- function(pd) {
  # Default empty result
  result <- list(
    function_definitions = character(0),
    function_calls = character(0),
    variable_assignments = character(0)
  )

  if (is.null(pd) || nrow(pd) == 0) {
    return(result)
  }

  # Extract function calls (SYMBOL_FUNCTION_CALL tokens)
  func_call_rows <- pd$token == "SYMBOL_FUNCTION_CALL"
  if (any(func_call_rows)) {
    result$function_calls <- unique(pd$text[func_call_rows])
  }

  # Find all assignment tokens
  assign_tokens <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")
  assign_rows <- which(pd$token %in% assign_tokens)

  if (length(assign_rows) == 0) {
    return(result)
  }

  func_defs <- character(0)
  var_assigns <- character(0)

  # Helper to get all descendants of a node
  get_descendants <- function(parent_id) {
    children <- pd[pd$parent == parent_id, ]
    if (nrow(children) == 0) {
      return(integer(0))
    }
    child_ids <- children$id
    for (cid in child_ids) {
      child_ids <- c(child_ids, get_descendants(cid))
    }
    child_ids
  }

  for (idx in assign_rows) {
    assign_id <- pd$id[idx]
    assign_parent <- pd$parent[idx]

    # Find sibling expr nodes (same parent as assignment)
    siblings <- pd[pd$parent == assign_parent & pd$token == "expr", ]

    if (nrow(siblings) == 0) {
      next
    }

    token_type <- pd$token[idx]
    assign_line1 <- pd$line1[idx]
    assign_col1 <- pd$col1[idx]

    # Determine LHS expr (before assignment) and RHS expr (after assignment)
    if (token_type == "RIGHT_ASSIGN") {
      # For RIGHT_ASSIGN: value -> target
      # Target expr is after assignment operator
      lhs_expr <- siblings[
        siblings$line1 > assign_line1 |
          (siblings$line1 == assign_line1 & siblings$col1 > assign_col1),
      ]
      rhs_expr <- siblings[
        siblings$line1 < assign_line1 |
          (siblings$line1 == assign_line1 & siblings$col1 < assign_col1),
      ]
    } else {
      # For LEFT_ASSIGN or EQ_ASSIGN: target <- value or target = value
      # Target expr is before assignment operator
      lhs_expr <- siblings[
        siblings$line1 < assign_line1 |
          (siblings$line1 == assign_line1 & siblings$col1 < assign_col1),
      ]
      rhs_expr <- siblings[
        siblings$line1 > assign_line1 |
          (siblings$line1 == assign_line1 & siblings$col1 > assign_col1),
      ]
    }

    if (nrow(lhs_expr) == 0) {
      next
    }

    # Get the first LHS expr (the target)
    lhs_expr_id <- lhs_expr$id[nrow(lhs_expr)]

    # Find the SYMBOL inside the LHS expr
    lhs_children <- pd[pd$parent == lhs_expr_id, ]
    target_symbol <- lhs_children[lhs_children$token == "SYMBOL", ]

    if (nrow(target_symbol) == 0) {
      next
    }

    target_name <- target_symbol$text[1]

    # Check if this is a function definition
    # Look for FUNCTION token in RHS expr or its descendants
    is_func_def <- FALSE
    if (nrow(rhs_expr) > 0) {
      rhs_expr_id <- rhs_expr$id[1]
      rhs_descendants <- get_descendants(rhs_expr_id)
      rhs_all <- pd[pd$id %in% c(rhs_expr_id, rhs_descendants), ]
      is_func_def <- any(rhs_all$token == "FUNCTION")
    }

    if (is_func_def) {
      func_defs <- c(func_defs, target_name)
    } else {
      var_assigns <- c(var_assigns, target_name)
    }
  }

  result$function_definitions <- unique(func_defs)
  result$variable_assignments <- unique(var_assigns)

  result
}

#' Analyze R Script
#'
#' Parse an R script and extract structural information including function
#' definitions, function calls, and variable assignments. Returns a CodeModel
#' object with the extracted features and computed musical parameters.
#'
#' @param file_path Path to the R script file to analyze
#' @return A CodeModel object containing extracted features and metrics
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze a script
#' model <- raver_analyze("my_script.R")
#' model$functions    # Function definitions
#' model$variables    # Variable assignments
#' model$part_count   # Musical parameter (1-4)
#' }
raver_analyze <- function(file_path) {
  # Validate file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }

  # Parse the file safely
  parsed <- safe_parse(file_path)

  # Get parse data
  pd <- getParseData(parsed)

  # Handle empty/NULL parse data (empty file or parse error)
  if (is.null(pd) || nrow(pd) == 0) {
    return(CodeModel$new(
      file_path = file_path,
      features = list(
        function_definitions = character(),
        function_calls = character(),
        variable_assignments = character()
      ),
      metrics = list(cyclomatic = 1L, nesting_depth = 0L, control_flow_count = 0L)
    ))
  }

  # Extract features from parse data
  features <- extract_features(pd)

  # Calculate complexity metrics from parse data
  metrics <- calculate_metrics(pd)

  # Create and return CodeModel
  CodeModel$new(file_path, features, metrics)
}
