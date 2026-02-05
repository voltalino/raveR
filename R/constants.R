#' @title Audio Constants
#' @description Standard audio parameters used throughout raveR.
#'   These constants ensure consistent Wave object creation and avoid
#'   format mismatches when binding/combining audio.
#' @name audio-constants
NULL

#' Sample rate in Hz (CD quality)
#'
#' @examples
#' # Use SAMPLE_RATE when creating Wave objects
#' SAMPLE_RATE
#'
#' @export
SAMPLE_RATE <- 44100L

#' Internal processing bit depth (32-bit float)
#'
#' @examples
#' # Check the internal bit depth
#' BIT_DEPTH
#'
#' @export
BIT_DEPTH <- 32L

#' PCM mode (FALSE = IEEE float for 32-bit processing)
#'
#' @examples
#' # Check PCM mode setting
#' PCM_MODE
#'
#' @export
PCM_MODE <- FALSE

#' Export bit depth (16-bit for CD quality WAV export)
#'
#' @examples
#' # Check export bit depth
#' EXPORT_BIT
#'
#' @export
EXPORT_BIT <- 16L
