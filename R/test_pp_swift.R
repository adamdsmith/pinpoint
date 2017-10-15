#' Test locational accuracy of PinPoint Swift fixes against a reference coordinate
#'
#' This function evaluates the locational accuracy of PinPoint Swift fixes by comparing
#'  them to an input reference coordinate (i.e., tag location in lat/long decimal degrees
#'  WGS84).  The most convenient input is a `pp_df` class data.frame produced by
#'  \code{\link{read_pp_swift}}. Failed GPS fix attempts are removed automatically.  Users
#'  can optionally filter GPS fixes by the reported horizontal dilution of precision `max_hdop`
#'  or the number of satellites `min_sats`.  Limited experimentation finds fixes with at least
#'  4 satellites and an HDOP <= 20 and to be of adequate locational quality, with an RMS of
#'  about 20 m (see details).  These are the defaults.  For larger scale studies, fixes from
#'  3 satellites may also be useful.
#'
#' The reported 2-D RMS error assumes that errors in the north-south and east-west directions
#'  are equivalent (i.e., that locational fixes describe a circular distribution around the
#'  true position; likely reasonable with >= 4 satellites).  Assuming also that mean positional
#'  error is zero (or close to it), RMS is essentially equivalent to 1 standard devation, which
#'  in the two-directional case means that a circle of radius RMS is expected to contain about
#'  63\% of positional locations on average; double it for the 95\% equivalent.  See Diggelen
#'  (2007) for more details: \url{http://gpsworld.com/gpsgnss-accuracy-lies-damn-lies-and-statistics-1134/})
#'
#' @param pp_df a `pp_df` object (i.e., a `data.frame` created by \code{\link{read_pp_swift}})
#' @param ref_coords a vector of coordinates (decimal degrees, WGS84 datum) of the reference location
#'  in the form \code{c(longitude, latitude)}.
#' @param min_sats numeric/integer scalar indicating the minimum number of satellites
#'  required (default is 4) to accept a GPS fix as valid.
#' @param max_hdop numeric scalar indicating the maximum dilution of precision (default is
#'  <= 20) to accept a GPS fix as valid.
#' @return A histogram of locational errors (with median highlighted) and a report of 2-D RMS
#'  error by tag ID, and a `pp_df` object containing the input data and two new columns:
#' \itemize{
#'   \item error_l - the absolute locational error, in meters
#'   \item error_b - the bearing of the locational error, in degrees from north
#' }
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' # Select apprpriate text files from file selection window that opens
#' pp_tests <- list.files(path = system.file("extdata", package = "pinpoint"), full.names = TRUE)
#' dat <- read_pp_swift(pp_tests)
#' dat <- test_pp_swift(dat, ref_coords = c(-83.360298, 33.895814))
#' dat <- test_pp_swift(dat, min_sats = 9, ref_coords = c(-83.360298, 33.895814))
#' }

test_pp_swift <- function(pp_df, ref_coords = c(-83.36, 33.95),
                          min_sats = 4, max_hdop = 20)
{
  if (inherits(pp_df, "data.frame")) {
    if (!any(c("tag_id", "lat", "lon", "hdop", "n_sats") %in% names(pp_df)))
      stop("At least one required column is missing.\n",
           "Was the output created by the `read_pp_swift` function?")
  } else stop("Input is not a `data.frame`. Run `read_pp_swift` function to generate input.")

  if (length(ref_coords) != 2 && !is.numeric(ref_coords))
    stop("Check your specification of the input reference coordinates `ref_coords`.")

  out <- pp_df %>%
    filter(.data$status == "valid",
           .data$hdop <= max_hdop,
           .data$n_sats >= min_sats)

  # Find distance and bearing between fixes and reference coordinates
  fix_sp <- cbind(out$lon, out$lat)
  ref_sp <- cbind(ref_coords[1], ref_coords[2])
  error_l <- geosphere::distVincentyEllipsoid(ref_sp, fix_sp) %>% round(1)
  error_b <- geosphere::bearing(ref_sp, fix_sp) %>% round(1)

  out <- data.frame(out, error_l, error_b)

  rms <- out %>% group_by(.data$tag_id) %>%
    summarize(n_fixes = n(),
              RMS_m = sqrt(mean(.data$error_l^2)))

  # Plot it
  p <- ggplot(out, aes(x = error_l)) +
    geom_histogram(binwidth = 3, colour="black", fill="white") +
    geom_vline(aes(xintercept = median(error_l)), color = "red", linetype = "dashed", size = 1) +
    facet_wrap(~tag_id, ncol = 3, scales = "free") + xlab("Absolute error (m)") +
    ylab("# fixes") + theme_bw()
  print(p)

  View(rms)

  class(out) <- c("pp_df", "data.frame")
  out

}
