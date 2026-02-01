#' @title Complexity Metrics
#' @description Calculate complexity metrics from R parse data for driving
#'   musical arrangement density and drum build intensity.
#' @name metrics
NULL

#' Calculate Complexity Metrics
#'
#' Calculate cyclomatic complexity, nesting depth, and control flow count
#' from R parse data. These metrics drive musical density in generated audio.
#'
#' @param pd Parse data frame from getParseData()
#' @return List with cyclomatic, nesting_depth, control_flow_count
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' expr <- parse(text = "for (i in 1:10) { if (i > 5) print(i) }", keep.source = TRUE)
#' pd <- getParseData(expr)
#' metrics <- calculate_metrics(pd)
#' metrics$cyclomatic       # Cyclomatic complexity
#' metrics$nesting_depth    # Maximum nesting depth
#' }
calculate_metrics <- function(pd) {
  # Default metrics for empty/NULL parse data
  default_metrics <- list(
    cyclomatic = 1L,
    nesting_depth = 0L,
    control_flow_count = 0L
  )

  if (is.null(pd) || nrow(pd) == 0) {
    return(default_metrics)
  }

  # Control flow tokens that add complexity
  # Based on McCabe's cyclomatic complexity for control flow

  control_tokens <- c("IF", "ELSE", "FOR", "WHILE", "REPEAT", "BREAK", "NEXT")

  # Count control flow statements
  control_flow_count <- sum(pd$token %in% control_tokens)

  # Simplified cyclomatic complexity: control flow + 1

  # McCabe's formula: V(G) = E - N + 2P, but for single function P=1
  # simplifies to number of decision points + 1
  cyclomatic <- control_flow_count + 1L


  # Calculate nesting depth
  nesting_depth <- calculate_nesting_depth(pd)

  list(
    cyclomatic = as.integer(cyclomatic),
    nesting_depth = as.integer(nesting_depth),
    control_flow_count = as.integer(control_flow_count)
  )
}

#' Calculate Maximum Nesting Depth
#'
#' Find the maximum depth of nested control structures in parse data.
#' Uses containment-based approach: compares parent expr line ranges
#' to determine which control structures are nested within others.
#'
#' @param pd Parse data frame from getParseData()
#' @return Integer maximum nesting depth (0 if no control structures)
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' expr <- parse(text = "for (i in 1:10) { for (j in 1:i) { print(j) } }",
#'               keep.source = TRUE)
#' pd <- getParseData(expr)
#' calculate_nesting_depth(pd)  # Returns 2
#' }
calculate_nesting_depth <- function(pd) {
  if (is.null(pd) || nrow(pd) == 0) {
    return(0L)
  }

  # Control tokens that create nesting (excluding ELSE, BREAK, NEXT)
  control_tokens <- c("IF", "FOR", "WHILE", "REPEAT")

  # Find all control nodes
  control_rows <- pd$token %in% control_tokens
  control_nodes <- pd[control_rows, c("id", "parent", "token"), drop = FALSE]

  if (nrow(control_nodes) == 0) {
    return(0L)
  }

  # Get parent expr line ranges for each control node
  # The parent expr contains the full control structure (including body)
  n_controls <- nrow(control_nodes)
  p_line1 <- integer(n_controls)
  p_col1 <- integer(n_controls)
  p_line2 <- integer(n_controls)
  p_col2 <- integer(n_controls)

  for (i in seq_len(n_controls)) {
    parent_id <- control_nodes$parent[i]
    parent_row <- pd[pd$id == parent_id, ]
    if (nrow(parent_row) > 0) {
      p_line1[i] <- parent_row$line1[1]
      p_col1[i] <- parent_row$col1[1]
      p_line2[i] <- parent_row$line2[1]
      p_col2[i] <- parent_row$col2[1]
    }
  }

  # For each control node, count how many other control structures contain it
  # Depth = number of containing structures + 1
  max_depth <- 1L

  for (i in seq_len(n_controls)) {
    containers <- 0L
    for (j in seq_len(n_controls)) {
      if (i == j) next

      # Check if control j contains control i
      # j contains i if j's range fully encompasses i's range
      starts_before <- p_line1[j] < p_line1[i] ||
        (p_line1[j] == p_line1[i] && p_col1[j] <= p_col1[i])
      ends_after <- p_line2[j] > p_line2[i] ||
        (p_line2[j] == p_line2[i] && p_col2[j] >= p_col2[i])

      if (starts_before && ends_after) {
        containers <- containers + 1L
      }
    }
    depth <- containers + 1L
    max_depth <- max(max_depth, depth)
  }

  max_depth
}
