#' Translate column names of a data.frame or lazy tibble using a dictionary
#'
#' Rename columns in a `data.frame` or `tbl_lazy` by mapping them through a
#' user-supplied dictionary. Columns not found in the dictionary are left
#' unchanged.
#'
#' @param d A `data.frame` or `tbl_lazy` object whose column names need to be
#'   translated.
#' @param dictionary A `data.frame` (or tibble) with at least two columns
#'   containing the current and replacement column names.
#' @param from A string naming the column in `dictionary` that contains the
#'   current (messy) column names. Defaults to `"messy"`.
#' @param to A string naming the column in `dictionary` that contains the
#'   replacement (clean) column names. Defaults to `"clean"`.
#'
#' @return An object of the same class as `d` with column names translated.
#'
#' @examples
#' dictionary <- data.frame(
#'   messy = c("vesselNumber", "GearIdentifier"),
#'   clean = c("vid", "gid")
#' )
#'
#' df <- data.frame(vesselNumber = 1, GearIdentifier = "OTB", year = 2025)
#' wk_translate(df, dictionary)
#'
#' @export
wk_translate <- function(d, dictionary, from = "messy", to = "clean") {
  if (!inherits(d, c("data.frame", "tbl_lazy")))
    stop("`d` must be either a data.frame or a tbl_lazy object.")
  if (!is.data.frame(dictionary))
    stop("`dictionary` must be a data.frame or tibble.")
  if (!all(c(from, to) %in% colnames(dictionary)))
    stop("`dictionary` must contain the specified `from` and `to` columns.")

  # names = new (to), values = old (from) — the convention dplyr::rename expects
  lookup <- stats::setNames(dictionary[[from]], dictionary[[to]])
  dplyr::rename(d, dplyr::any_of(lookup))
}
