read_tidy_pp <- function(swift_txt, out_tz = "America/New_York", valid_only = FALSE) {
  suppressWarnings(
    out <- readr::read_fwf(
      swift_txt,
      readr::fwf_empty(swift_txt, skip = 1,
                       col_names = c("index", "status", "n_sats", "rtc_date",
                                     "rtc_time", "fix_date", "fix_time", "delta_s",
                                     "lat", "lon", "alt", "hdop", "eres")),
      col_types = "icccccc_ddddd", skip = 1) %>%
    mutate(
      tag_id = as.integer(gsub("(^.*PinPoint )(\\d+)( .+$)", "\\2", swift_txt)),
      date = as.Date(ymd_hms(paste(.data$rtc_date, .data$rtc_time), tz = "GMT"),
                     tz = out_tz),
      sched_local = ymd_hm(paste(date, substr(.data$rtc_time, 1, 5)), tz = "GMT"),
      fix_GMT = ymd_hms(paste(.data$fix_date, .data$fix_time), tz = "GMT"),
      fix_local = ymd_hms(paste(.data$fix_date, .data$fix_time), tz = "GMT"),
      n_sats = as.integer(sub("/.*", "", .data$n_sats)),
      status = ifelse(.data$status == "Valid", "valid", "invalid")) %>%
    select(.data$tag_id, .data$status, .data$n_sats, .data$date, .data$sched_local,
            .data$fix_local, .data$fix_GMT, .data$lat, .data$lon, .data$alt,
            .data$hdop, .data$eres)
  )
  attr(out$sched_local, "tzone") <- attr(out$fix_local, "tzone") <- out_tz
  if (valid_only) out <- filter(out, .data$status == "valid")
  as.data.frame(out)
}
