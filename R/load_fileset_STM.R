#' @title System tool: download filesets using parallel computation
#'
#' @description
#' Similar to `wx3::load_fileset`, but forced to have parallel computation.
#'
#' @param data A `dataframe` with character columns of `URL`, `DIR`, and `Info`
#' @param worker (`numerical`) Number of workers to download the data
#' @param attempt (`numerical`) The maximum number of download attempts
#' @param threshold (`numerical`) The minimum file size in bytes
#' @param list_fail (`logical`) List failed to download items
#' @param title (`character`) Title of the downloaded items
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return
#' @keywords internal
load_fileset.STM = function(data, worker, attempt, threshold, list_fail, title, ...){
  #Start ####
  tmp_dir = paste0(tempdir(), "/wx3")
  dir.create(tmp_dir, showWarnings = FALSE)
  data_download = dplyr::filter(.data = wx3:::load_file.format(data = data),
                                !Exist)
  #Download panel ####
  list_attempt = vector(mode = "numeric", length = attempt)
  time_start = Sys.time()
  files = unique(data$DIR)
  data_1 = tryCatch(basename(files[[1]]), error = function(e){NA})
  data_n = ifelse(is.na(data_1), NA, basename(files[[length(files)]]))

  wx3:::load_panelstart(title = title,
                        time_start = time_start,
                        data_1 = data_1,
                        data_n = data_n,
                        data_num = length(files),
                        storm = TRUE)

  future::plan("future::multisession", workers = worker)
  for(i in seq_len(attempt)){
    if(nrow(data_download) > 0){
      cli::cli_text("{i} Attempt | {length(files)} Fileset{?s} | {future::nbrOfWorkers()} Worker{?s}:")
      list_attempt[[i]] = nrow(data_download)
      wx3:::load_fileset.STMengine(data_download = data_download,
                                   tmp_dir = tmp_dir,
                                   threshold = threshold,
                                   ... = ...)
      data_download = wx3:::load_file.format(data_download)
      data_summary  = dplyr::distinct(dplyr::select(.data = data_download, -URL))

      cli::cli_text("{sum(data_summary$Exist)} Success{?es} | {sum(!data_summary$Exist)} Fail{?s}")
      data_download = dplyr::filter(.data = data_download, !Exist)
    }
  }
  future::plan("future::sequential")

  data = dplyr::distinct(dplyr::select(.data = wx3:::load_file.format(data), -URL))
  time_end = Sys.time()

  wx3:::load_panelend(time_start = time_start,
                      time_end = time_end,
                      attempts = list_attempt,
                      success = sum(data$Exist),
                      fail = sum(!(data$Exist)),
                      list_fail = list_fail,
                      list_of_fail = data$Info[!(data$Exist)])
}

#' @title System tool: Engine of the load_fileset function
#'
#' @description
#' The engine to download filesets from a site, using parallel computation.
#'
#' @param data_download (`data.frame`) Data to be downloaded, similar to `data` in `wx3::load_fileset`
#' @param tmp_dir (`character`) Temporary directory of the main `R` session
#' @param threshold (`numerical`) The minimum file size in bytes, similar to `threshold` in `wx3::load_fileset`
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return
#' @keywords internal
load_fileset.STMengine = function(data_download, tmp_dir, threshold, ...){
  control = TRUE
  list_DIR = unique(data_download$DIR)
  list_TMP = vector(mode = "character", length = length(list_DIR))

    for(i in seq_len(length(list_DIR))){
      progressr::handlers(progressr::handler_cli(format = stringr::str_flatten(c("{cli::pb_spin} ", i, "/", length(list_DIR),
                                                                                 " {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"))))
      download_attempt = dplyr::filter(.data = data_download, DIR == list_DIR[[i]])
      nrow = nrow(download_attempt)
      steps = round(x = nrow, digits = -floor(log10(nrow)))/20

      progressr::with_progress({
        p = progressr::progressor(steps = nrow/steps, auto_finish = FALSE, initiate = TRUE)
        download_success = furrr::future_map(.x = seq_len(nrow(download_attempt)),
                                             .f = function(.x, steps, download_attempt, tmp_dir, threshold, env, ...){
                                               if(round(.x %% steps) == 1){p()}
                                               if(get(x = "control", envir = env)){
                                                 url_x = download_attempt$URL[[.x]]
                                                 dir_x = download_attempt$DIR[[.x]]
                                                 tmp_x = stringr::str_flatten(string = c(tmp_dir,
                                                                                         "/", .x, "_", basename(dir_x)))
                                                 tryCatch(writeBin(RCurl::getBinaryURL(url = url_x, ... = ...), con = tmp_x),
                                                          error = function(e){})
                                                 if(file.exists(tmp_x)){
                                                   if(file.size(tmp_x) >= threshold){
                                                     assign(x = "control", value = FALSE, envir = env)
                                                     return(tmp_x)
                                                   }
                                                 }
                                               }
                                               return(NA)},
                                           steps = steps, download_attempt = download_attempt,
                                           tmp_dir = tmp_dir, threshold = threshold,
                                           env = environment(), ... = ...)
      })
      download_success = download_success[!is.na(download_success)]

      list_TMP[[i]] = ifelse(length(download_success), download_success[[1]], NA)
      tmp_dir_files = list.files(path = tmp_dir, full.names = TRUE)
      file.remove(tmp_dir_files[!(tmp_dir_files %in% list_TMP)])
    }

  tmp_DIR = dplyr::filter(.data = data.frame(DIR = list_DIR, TMP = list_TMP), !is.na(TMP))
  for(X in unique(dirname(tmp_DIR$DIR))){dir.create(path = X, showWarnings = FALSE, recursive = TRUE)}
  file.rename(from = tmp_DIR$TMP, to = tmp_DIR$DIR)
}
