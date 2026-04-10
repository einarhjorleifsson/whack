#' Convert degrees-minutes-seconds to decimal degrees
#'
#' Converts values encoded as `ddmmss` (degrees-minutes-seconds) into decimal
#' degrees.
#'
#' The encoding is `dd * 10000 + mm * 100 + ss`, where:
#' - `dd` is (absolute) degrees (0–180)
#' - `mm` is whole minutes (00–59)
#' - `ss` is seconds (0–59.999...)
#'
#' For example, `65595930` encodes 65°59'59.30". Non-integer inputs are
#' supported: the fractional part of `x` extends the seconds beyond whole
#' numbers. For instance, `655959.5` encodes 65°59'59.5".
#'
#' The sign of `x` is preserved in the output. Values with fewer than 6 digits
#' are interpreted as right-aligned `ddmmss` components (e.g. `30` is treated
#' as 30 seconds, `5959` as 59 minutes 59 seconds, `10000` as 1 degree).
#'
#' When `validate = TRUE`, the function checks that minutes are in `[0, 59]`,
#' seconds are in `[0, 60)`, and absolute degrees are `<= 180`; otherwise it
#' throws an error.
#'
#' @param x Numeric vector encoded as `ddmmss`.
#' @param validate Logical (default `TRUE`). If `TRUE`, validates that minutes
#'   are in `[0, 59]`, seconds are in `[0, 60)`, and degrees are `<= 180`.
#'
#' @return A numeric vector of decimal degrees (type `double`), with `NA_real_`
#'   where `x` is `NA`.
#'
#' @examples
#' x <- c(-30, -5959, -65559, -173000)
#' wk_convert_dms(x)
#' # -0.008333333  -0.999722222  -6.933055556 -17.500000000
#'
#' # Validation: minutes/seconds must be in [0, 59]
#' \dontrun{
#' wk_convert_dms(c(126199))  # 61 minutes -> error
#' }
#'
#' @export
wk_convert_dms <- function(x, validate = TRUE) {
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
    bad <- mm >= 60 |
      ss >= 60 |
      dd > 180
    if (any(bad)) {
      stop(
        "Invalid DMS values at positions: ",
        paste(which(ok)[bad], collapse = ", ")
      )
    }
  }

  out[ok] <- s * (dd + mm / 60 + ss / 3600)
  out
}



#' Convert degrees-decimal minutes to decimal degrees
#'
#' Converts values encoded as `ddmmdm` (degrees and decimal minutes) into
#' decimal degrees.
#'
#' The encoding is `dd * 10000 + decimal_minutes * 100`, where:
#' - `dd` is (absolute) degrees (0–180)
#' - `decimal_minutes` is minutes expressed as a decimal number (0–59.999...)
#'
#' For example, `643050` encodes 64 degrees and 30.50 minutes
#' (64 * 10000 + 30.50 * 100). Non-integer inputs are supported: the fractional
#' part of `x` extends precision beyond hundredths of a minute. For instance,
#' `643050.99` encodes 64 degrees and 30.5099 minutes.
#'
#' The sign of `x` is preserved in the output.
#'
#' When `validate = TRUE`, the function checks that the minutes component
#' (i.e. `x %% 10000`) is less than `6000` (i.e. < 60 minutes) and that
#' absolute degrees are `<= 180`; otherwise it throws an error.
#'
#' @param x Numeric vector encoded as `ddmmdm`.
#' @param validate Logical (default `TRUE`). If `TRUE`, validates that minutes
#'   are in `[0, 60)` and degrees are `<= 180`.
#'
#' @return A numeric vector of decimal degrees (type `double`), with `NA_real_`
#'   where `x` is `NA`.
#'
#' @seealso [geo::geoconvert.1()], from which the conversion formula is
#'   directly adapted.
#'
#' @examples
#' # 64°30.50' N and 22°45.00' W (integer input)
#' wk_convert_ddm(c(643050, -224500))
#' # 64.50833  -22.75000
#'
#' # Decimal input: 643050.99 encodes 64°30.5099' (sub-hundredth precision)
#' wk_convert_ddm(643050.99)
#' # 64.50850
#'
#' # Validation: minutes must be < 60
#' \dontrun{
#' wk_convert_ddm(6000)  # 60 minutes -> error
#' }
#'
#' @export
wk_convert_ddm <- function(x, validate = TRUE) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) return(out)

  xi <- x[ok]
  s <- sign(xi)
  xi <- abs(xi)

  mm_dm <- xi %% 10000  # last 4 digits: whole minutes * 100 + decimal minutes

  if (validate) {
    bad <- mm_dm >= 6000 | xi %/% 10000 > 180
    if (any(bad)) {
      stop(
        "Invalid DDM values at positions: ",
        paste(which(ok)[bad], collapse = ", ")
      )
    }
  }

  min <- (xi / 100) - trunc(xi / 10000) * 100
  out[ok] <- s * (xi + (200 / 3) * min) / 10000
  out
}
