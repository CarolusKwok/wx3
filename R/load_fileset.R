#' @title Download files from URL to Directory
#'
#' @description
#' Portal to download a list of file sets, using sequential and parallel programming. A file set refers to files with multiple, potential, URLs. These can be extremely common as some URLs contains the date and time of when these URLs are created.
#'
#' To initiate download process, input a dataframe with columns `DIR`, `URL`, `Info` representing the url, the destination directory and info of the data desired into `data`. The switch between sequential and parallel are determined by number of workers set
#'
#' If ...
#' * `worker` < 0, Returns the dataframe
#' * `worker` = 1, Initiate the download using sequential computation
#' * `worker` > 1, Initiate the download using parallel computation
#'
#' The download files may be incomplete, due to network issues. `threshold` determines the min. file size to be download. Any files with a smaller file size will be rejected. `attempt` determines the number of attempts to download a certain file. The next attempt(s) will only attempt to download the failed files in the current attempt. `list_fail` will list all failed download attempts.
#'
#' @param data (data.frame) Dataframe with character columns of `URL`, `DIR`, and `Info`. Read the description for more information.
#' @param worker (integer) Number of workers to download the data
#' @param attempt (integer) The maximum number of download attempts
#' @param threshold (numerical) The minimum file size in bytes
#' @param list_fail (logical) List failed to download items
#' @param title (character) Title of the downloaded items
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return Downloaded files
#' @export
load_fileset = function(data,
                        worker = 0,
                        attempt = 5,
                        threshold = 1,
                        list_fail = TRUE,
                        title = "test",
                        ...){
  #Check ####
  if(rlang::is_missing(data)){call::abort_NoArg(data)}
  if(rlang::is_missing(worker)){call::abort_NoArg(worker)}
  if(rlang::is_missing(attempt)){call::abort_NoArg(attempt)}
  if(rlang::is_missing(threshold)){call::abort_NoArg(threshold)}
  if(rlang::is_missing(list_fail)){call::abort_NoArg(list_fail)}
  if(rlang::is_missing(title)){call::abort_NoArg(title)}

  if(!is.data.frame(data)){call::abort_WrongClass(x = data, class = "data.frame")}
  if(!is.integer(worker)){call::abort_WrongClass(x = worker, class = "integer")}
  if(!is.integer(attempt)){call::abort_WrongClass(x = attempt, class = "integer")}
  if(!is.numeric(threshold)){call::abort_WrongClass(x = threshold, class = "numeric")}
  if(!is.logical(list_fail)){call::abort_WrongClass(x = list_fail, class = "logical")}
  if(!is.character(title)){call::abort_WrongClass(x = title, class = "character")}

  colnames = colnames(data)
  required_colnames = c("URL", "DIR", "Info")
  if (sum(!(required_colnames %in% colnames))){
    call::abort(
      message = c(
        "x" = "{.arg data} must contain certain columns",
        "i" = "{.arg data} contains the following columns",
        "i" = call::msg_column(colnames),
        "i" = "{.arg data} requries the following columns",
        "i" = call::msg_column(required_colnames)
      )
    )
  }

  #Return the dataframe if worker == 0
  #Else Start
  if(worker <= 0){
    return(data)
  } else {
    #Start ####
    tmp_dir = paste0(tempdir(), "/wx3")
    dir.create(tmp_dir, showWarnings = FALSE)
    data_download = dplyr::filter(.data = wx3:::load_file.format(data = data),
                                  !Exist)

    #Show Panel - Start download####
    list_attempt = vector(mode = "numeric", length = attempt)
    time_start = Sys.time()
    files = unique(data$DIR)
    data_1 = tryCatch(
      basename(files[[1]]),
      error = function(e) {
        NA
      }
    )
    data_n = ifelse(is.na(data_1), NA, basename(files[[length(files)]]))

    wx3:::load_panelstart(
      title = title,
      time_start = time_start,
      data_1 = data_1,
      data_n = data_n,
      data_num = length(files),
      storm = TRUE
    )

    #Allocate correct workers & DOWNLOAD####
    if (worker == 1) {
      future::plan("future::sequential")
    } else {
      future::plan("future::multisession", workers = worker)
    }

    #Start ####
    for (i in seq_len(attempt)) {
      if (nrow(data_download) > 0) {
        cli::cli_text("{i} Attempt | {length(files)} Fileset | {future::nbrOfWorkers()} Worker:")
        list_attempt[[i]] = nrow(data_download)
        wx3:::load_fileset.engine(
          data_download = data_download,
          tmp_dir = tmp_dir,
          threshold = threshold,
          ... = ...
        )

        data_download = wx3:::load_file.format(data_download)
        data_summary  = dplyr::distinct(dplyr::select(.data = data_download, -URL))
        cli::cli_text("{sum(data_summary$Exist)} Success | {sum(!data_summary$Exist)} Fail")
        data_download = dplyr::filter(.data = data_download, !Exist)
      }
    }

    #Allocate correct workers ####
    future::plan("future::sequential")

    #Show Panel - End download####
    data = dplyr::distinct(dplyr::select(.data = wx3:::load_file.format(data), -URL))
    time_end = Sys.time()
    wx3:::load_panelend(
      time_start = time_start,
      time_end = time_end,
      attempts = list_attempt,
      success = sum(data$Exist),
      fail = sum(!(data$Exist)),
      list_fail = list_fail,
      list_of_fail = data$Info[!(data$Exist)]
    )
  }
}
