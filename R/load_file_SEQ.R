#' @title System tool: download files using sequential computation
#'
#' @description
#' Similar to `wx3::load_file`, but forced to have sequential computation.
#'
#' @param data A `dataframe` with character columns of `URL`, `DIR`, and `Info`
#' @param attempt The maximum number of download attempts (`numerical`)
#' @param threshold The minimum file size in bytes (`numerical`)
#' @param list_fail List failed to download items (`logical`)
#' @param title Title of the downloaded items (`character`)
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return
#' @keywords internal
load_file.SEQ = function(data, worker, attempt, threshold, list_fail, title, ...){
  #Start ####
  tmp_dir = paste0(tempdir(), "/wx3")
  dir.create(tmp_dir, showWarnings = FALSE)
  data_download = dplyr::filter(.data = wx3:::load_file.format(data = data),
                                !Exist)
  #Download panel ####
  list_attempt = vector(mode = "numeric", length = attempt)
  time_start = Sys.time()
  data_1 = tryCatch(basename(data_download$DIR[[1]]), error = function(e){NA})
  data_n = ifelse(is.na(data_1), NA, basename(data_download$DIR[[nrow(data_download)]]))

  wx3:::load_panelstart(title = title,
                        time_start = time_start,
                        data_1 = data_1,
                        data_n = data_n,
                        data_num = nrow(data_download),
                        storm = FALSE)

  future::plan("future::sequential")
  for(i in seq_len(attempt)){
    if(nrow(data_download) > 0){
      cli::cli_text("{i} Attempt | {nrow(data_download)} File{?s}:")
      list_attempt[[i]] = nrow(data_download)
      wx3:::load_file.SEQengine(data_download = data_download,
                                tmp_dir = tmp_dir,
                                threshold = threshold,
                                ... = ...)
      data_download = wx3:::load_file.format(data_download)
      cli::cli_text("{sum(data_download$Exist)} Success{?es} | {sum(!data_download$Exist)} Fail{?s}")
      data_download = dplyr::filter(.data = data_download, !Exist)
    }
  }

  data = wx3:::load_file.format(data)
  time_end = Sys.time()

  wx3:::load_panelend(time_start = time_start,
                      time_end = time_end,
                      attempts = list_attempt,
                      success = sum(data$Exist),
                      fail = sum(!(data$Exist)),
                      list_fail = list_fail,
                      list_of_fail = data$Info[!(data$Exist)])
}

#' @title System tool: Sequential engine of the load_file function
#'
#' @description
#' The engine to download files from a site, using sequential computation.
#'
#' @param data_download Data to be downloaded, similar to `data` in `wx3::load_file`
#' @param tmp_dir Temporary directory of the main `R` session
#' @param threshold  The minimum file size in bytes (`numerical`), similar to `threshold` in `wx3::load_file`
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return
#' @keywords internal
load_file.SEQengine = function(data_download, tmp_dir, threshold, ...){
  progressr::handlers(progressr::handler_cli(format = "{cli::pb_spin} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"))

  progressr::with_progress({
    nrow = nrow(data_download)
    steps = round(x = nrow, digits = -floor(log10(nrow)))/20
    p = progressr::progressor(steps = nrow/steps)
    download_success = purrr::map(.x = seq_len(nrow(data_download)),
                                  .f = function(.x, data_download, tmp_dir, threshold, steps, ...){
                                    if(round(.x %% steps) == 1){p()}
                                    dir_x = data_download$DIR[[.x]]
                                    url_x = data_download$URL[[.x]]
                                    tmp_x = stringr::str_flatten(string = c(tmp_dir,
                                                                            "/", wx3:::sys_info(), "_",
                                                                            basename(dir_x)))
                                    tryCatch(writeBin(RCurl::getBinaryURL(url = url_x, ... = ...), con = tmp_x),
                                             error = function(e){})
                                    if(file.exists(tmp_x)){
                                      if(file.size(tmp_x) >= threshold){return(tmp_x)} else{file.remove(tmp_x)}
                                      }
                                    return(NA)},
                                  data_download = data_download, tmp_dir = tmp_dir, threshold = threshold, steps = steps, ... = ...)
  })

  data_download = dplyr::filter(.data = dplyr::mutate(data_download,
                                                      success = unlist(download_success)),
                                !is.na(success))
  for(X in unique(dirname(data_download$DIR))){
    dir.create(path = X, showWarnings = FALSE, recursive = TRUE)
  }
  file.rename(from = data_download$success, to = data_download$DIR)
}
