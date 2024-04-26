#' @title
#' Engine of the load_file function
#'
#' @description
#' The engine to download files from a site, using parallel computation.
#'
#' @param data_download (dataframe) Data to be downloaded, similar to `data` in `wx3::load_file`
#' @param tmp_dir (character) Temporary directory of the main `R` session
#' @param threshold (numeric) The minimum file size in bytes, similar to `threshold` in `wx3::load_file`
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return NA
#' @keywords internal
load_file.engine = function(data_download, tmp_dir, threshold, ...){
  tmp_file = list.files(tmp_dir, full.names = TRUE)
  file.remove(tmp_file)

  data_download = dplyr::mutate(.data = data_download,
                                TMP = paste0(tmp_dir, "/", dplyr::row_number(), "_", basename(DIR)))

  progressr::handlers(progressr::handler_cli(format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"))
  progressr::with_progress({
    nrow = nrow(data_download)
    steps = round(x = nrow, digits = -floor(log10(nrow)))/20
    p = progressr::progressor(steps = nrow/steps)
    stat = furrr::future_map(.x = seq_len(nrow(data_download)),
                             .f = function(.x, data_download, steps, ...){
                               if(round(.x %% steps) == 1){p()}
                               tmp_x = data_download$TMP[[.x]]
                               url_x = data_download$URL[[.x]]
                               stat = wx3:::load(url = url_x, dir = tmp_x, ...)
                               return(stat)
                             },
                             data_download = data_download, steps = steps, ... = ...)
  })

  if("#FAIL" %in% stat){
    call::warn(message = c("x" = "Error: Download incomplete",
                           "i" = "Error due to network failure at any point of this attempt",
                           "i" = "No files are created"))
  } else {
    data_download = dplyr::filter(.data = dplyr::mutate(.data = data_download,
                                                        Size_tmp = file.size(TMP)),
                                  Size_tmp >= threshold)
    if(nrow(data_download) > 0){
      for(X in unique(dirname(data_download$DIR))){dir.create(path = X, showWarnings = FALSE, recursive = TRUE)}
      file.rename(from = data_download$TMP, to = data_download$DIR)
    }
  }

  tmp_file = list.files(tmp_dir, full.names = TRUE)
  file.remove(tmp_file)
}
