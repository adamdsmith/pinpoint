read_tidy_pp <- function(swift_txt, out_tz = "America/New_York", valid_only = FALSE) {
  out <- suppressWarnings(
    readr::read_fwf(swift_txt, readr::fwf_empty(swift_txt, skip = 1,
                                                col_names = c("index", "status", "n_sats", "rtc_date",
                                                              "rtc_time", "fix_date", "fix_time", "delta_s",
                                                              "lat", "lon", "alt", "hdop", "eres")),
                    col_types = "icccccc_ddddd", skip = 1)) %>%
    dplyr::mutate_(
      tag_id = ~ gsub("(^.*PinPoint )(\\d+)( .+$)", "\\2", swift_txt),
      date = ~as.Date(ymd_hms(paste(rtc_date, rtc_time), tz = "GMT"), tz = out_tz),
      sched_local = ~ymd_hm(paste(date, substr(rtc_time, 1, 5)), tz = "GMT"),
      fix_GMT = ~suppressWarnings(ymd_hms(paste(fix_date, fix_time), tz = "GMT")),
      fix_local = ~suppressWarnings(ymd_hms(paste(fix_date, fix_time), tz = "GMT")),
      n_sats = ~as.integer(sub("/.*", "", n_sats)),
      status = ~ifelse(status == "Valid", "valid", "invalid")) %>%
    dplyr::select_(quote(tag_id), quote(status), quote(n_sats), quote(date),
                   quote(sched_local), quote(fix_local), quote(fix_GMT),
                   quote(lat), quote(lon), quote(alt), quote(hdop), quote(eres))
  attr(out$sched_local, "tzone") <- attr(out$fix_local, "tzone") <- out_tz
  if (valid_only) out <- dplyr::filter_(out, ~ status == "valid")
  as.data.frame(out)
}
