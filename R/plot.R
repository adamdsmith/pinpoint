#' Plot processed Swift fixes from PinPoint Host software output
#'
#' This function uses \code{\link[leaflet]{leaflet}} to generate a basic interactive map of
#'  PinPoint Swift fixes from the `pp_df` class data produced by \code{\link{read_pp_swift}}.
#'  A separate overlay group is created for each distinct tag ID.  Failed GPS
#'  fix attempts are removed automatically.  Users can optionally filter GPS fixes by the
#'  reported dilution of precision `max_hdop` or the number of satellites `min_sats`.
#'  Limited experimentation finds fixes with at least 4 satellites and an HDOP <= 20 and to
#'  be of adequate locational quality, with an RMS of about 20 m (see details of
#'  \code{\link{test_pp_swift}} for more information).  These are the defaults.  For larger
#'  scale studies, fixes from 3 satellites may also be useful.
#'
#' @param pp_df a `pp_df` object (i.e., a `data.frame` created by \code{\link{read_pp_swift}})
#' @param min_sats numeric/integer scalar indicating the minimum number of satellites
#'  required (default is 4) to accept a GPS fix as valid.
#' @param max_hdop numeric scalar indicating the maximum dilution of precision (default is
#'  <= 20) to accept a GPS fix as valid.
#' @import leaflet
#' @export
#' @examples
#' \dontrun{
#' # Select apprpriate text files from file selection window that opens
#' dat <- read_pp_swift() <- Sys.time()
#' plot(dat)
#' plot(dat, min_sats = 6)
#' }

plot.pp_df <- function(pp_df, min_sats = 4, max_hdop = 20)
{
  if (inherits(pp_df, "data.frame")) {
    if (!any(c("tag_id", "lat", "lon", "hdop", "n_sats") %in% names(pp_df)))
      stop("At least one required column is missing.\n",
           "Was the output created by the `read_pp_swift` function?")
  } else stop("Input is not a `data.frame`. Run `read_pp_swift` function to generate input.")

  pp_df <- pp_df %>%
    dplyr::filter_(~status == "valid") %>%
    dplyr::filter_(~hdop <= max_hdop) %>%
    dplyr::filter_(~n_sats >= min_sats)

  # Get timezone of fixes
  tz <- attributes(pp_df[["fix_local"]])$tzone

  # Set up separate overlays/colors by tag
  tags <- unique(pp_df$tag_id)
  colors = viridis::viridis(length(tags))
  tag_colors = colorFactor(palette = colors, domain = pp_df$tag_id)

  p <- leaflet(pp_df) %>%

    # Base map group
    addProviderTiles("Esri.WorldImagery", group = "Aerial",
                     options = tileOptions(minZoom=3, maxNativeZoom=19,
                                                    maxZoom=22)) %>%
    addProviderTiles("Stamen.Terrain", group = "Terrain",
                     options = tileOptions(minZoom=3))




    for (tag in tags) {
    d <- pp_df[pp_df$tag_id == tag, ]
    p <- p %>% addCircleMarkers(data = d, lng = ~lon, lat = ~lat,
                                fillColor = ~tag_colors(tag_id),
                                fillOpacity = 1, radius = 5,
                                color = "black", weight = 1, opacity = 1,
                                popup = ~paste(paste("Tag ID:", tag_id),
                                               paste("# Sats:", n_sats),
                                               paste("HDOP:", hdop),
                                               paste(fix_local, tz),
                                               sep = "<br/>"),
                                group = tag)
    }

  p %>% addLayersControl(baseGroups = c("Aerial", "Terrain"),
                         overlayGroups = tags,
                         options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(position = "bottomright")

}
