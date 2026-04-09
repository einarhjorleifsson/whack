#' Convert signed ddmmss integer encoding to decimal degrees
#'
#' Converts values encoded as `ddmmss` (degrees-minutes-seconds) into decimal
#' degrees.
#'
#' The input is typically an integer with up to 6 digits where:
#' \itemize{
#'   \item `dd` is (absolute) degrees (0–180)
#'   \item `mm` is minutes (0–59)
#'   \item `ss` is seconds (0–59)
#' }
#'
#' The sign of `x` is preserved in the output. Values with fewer than 6 digits
#' are interpreted as right-aligned `ddmmss` components (e.g. `30` is treated as
#' 30 seconds, `5959` as 59 minutes 59 seconds, `10000` as 1 degree).
#'
#' When `validate = TRUE`, the function checks that minutes and seconds are in
#' `[0, 59]` and that absolute degrees are `<= 180`; otherwise it throws an error.
#'
#' @param x A numeric/integer vector encoded as signed `ddmmss`.
#' @param validate Boolean (default TRUE).
#'
#' @return A numeric vector of decimal degrees (type `double`), with `NA_real_`
#'   where `x` is `NA`.
#'
#' @examples
#' x <- c(-30, -5959, -65559, -173000)
#' wk_convert_ddmmss(x)
#' # -0.008333333  -0.999722222  -6.933055556 -17.500000000
#'
#' # Validation: minutes/seconds must be in 0..59
#' \dontrun{
#' wk_convert_ddmmss(c(126199))  # 61 minutes -> error
#' }
#'
#' @export
wk_convert_ddmmss <- function(x, validate = TRUE) {
  # preserve NA
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) return(out)

  xi <- x[ok]
  s <- sign(xi)
  xi <- abs(xi)

  dd <- xi %/% 10000L
  mm <- (xi %/% 100L) %% 100L
  ss <- xi %% 100L

  if (validate) {
    bad <- (mm < 0L | mm > 59L) |
      (ss < 0L | ss > 59L) |
      (dd > 180L)
    if (any(bad)) {
      stop(
        "Invalid ddmmss values at positions: ",
        paste(which(ok)[bad], collapse = ", ")
      )
    }
  }

  out[ok] <- s * (dd + mm / 60 + ss / 3600)
  out
}

