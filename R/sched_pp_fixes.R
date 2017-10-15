#' Lotek PinPoint schedule creation
#'
#' This function generates a **discrete** GPS sampling/fix schedule for PinPoint
#'  tags in the appropriate .ASF XML format.  The resulting file can then be loaded
#'  to the tag via the PinPoint Host software (see that manual for details on
#'  scheduling).
#'
#' @param date_times either character or POSIXct vector of date times for which
#'  to schedule an attempted GPS fix
#' @param tz character string indicating the \code{\link[base]{timezone}} of *input*
#'  datetimes. These will be converted to GMT as required by the PinPoint tags.
#' @param out_file character path to output XML file containing the discrete rule
#'  schedule.  The .ASF extension need not be specified.
#' @return writes an XML file (*.ASF extension) that can be loaded to a PinPoint tag
#'  via the PinPoint Hose software from Lotek
#' @export
#' @examples
#' now <- Sys.time()
#' dt_seq <- seq(from = now, length.out = 5, by = "6 hours")
#'
#' # Generate schedule 'test.ASF' in current directory
#' sched_pp_fixes(dt_seq, tz = "America/New_York", out_file = "./test")

sched_pp_fixes <- function(date_times, tz = "GMT",
                         out_file = "./discrete_pp") {
  if (tz != "GMT") {
    message("\nPinPoint schedules require datetimes in GMT.\n",
            "Your input datetimes will be converted assuming your input datetimes\n",
            "have been specified as from the **", tz, "** time zone.\n",
            "Please verify to avoid anger issues later.\n")
    attr(date_times, "tzone") <- "GMT"
  }

  date_times <- sort(date_times)
  uniq_dates <- as.Date(date_times) %>% unique()

  rules <- lapply(uniq_dates, function(day) {

    valid_dts <- date_times[as.Date(date_times) == day]
    times <- paste(format(valid_dts, format = "%H:%M"), collapse = ";")

    c("\t<rule>",
      "\t\t<type>Discrete</type>",
      paste0("\t\t<firstday>", day, "</firstday>"),
      paste0("\t\t<lastday>", day, "</lastday>"),
      paste0("\t\t<dailyevents>", times, ";</dailyevents>"),
      "\t</rule>"
      )

  })

  schedule_st <- c(paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>"),
                   "<schedule>")
  schedule_end <- "</schedule>"

  out <- c(schedule_st, unlist(rules), schedule_end)

  if (tools::file_ext(out_file) != "ASF") out_file <- paste0(out_file, ".ASF")

  conn <- file(out_file)
  writeLines(out, conn)
  close(conn)
  message("PinPoint GPS logger schedule written to:\n",
          normalizePath(out_file, "/"))

  invisible()

}
