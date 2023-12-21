#' @title Download files from URL to Directory
#'
#' @description
#' Portal to download a list of files, using sequential and parallel programming. To initiate download process, input a dataframe with columns `DIR`, `URL`, `Info` representing the url, the destination directory and info of the data desired into `data`. The switch between sequential and parallel are determined by number of workers set.
#' If ...
#' * `worker` < 0, Returns the dataframe
#' * `worker` = 1, Initiate the download using sequential computation
#' * `worker` > 1, Initiate the download using parallel computation
#'
#' The download files may be incomplete, due to networking issues. `threshold` determines the min. file size to be download. Any files with a smaller file size will be rejected. `attempt` determines the number of attempts to download a certain file. The next attempt(s) will only attempt to download the failed files in the current attempt. `list_fail` will list all failed download attempts.
#'
#' @param data A `dataframe` with character columns of `URL`, `DIR`, and `Info`
#' @param worker Number of workers to download the data (`numerical`)
#' @param attempt The maximum number of download attempts (`numerical`)
#' @param threshold The minimum file size in bytes (`numerical`)
#' @param list_fail List failed to download items (`logical`)
#' @param title Title of the downloaded items (`character`)
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return Downloaded files
#' @export
load_file = function(data,
                     worker = 0,
                     attempt = 5,
                     threshold = 1,
                     list_fail = TRUE,
                     title = "test",
                     ...){
  #Return the dataframe if worker == 0
  if(worker <= 0){
    return(data)
  }
  #Start
  if(worker == 1){
    wx3:::load_file.SEQ(data = data,
                        worker = worker,
                        attempt = attempt,
                        threshold = threshold,
                        list_fail = list_fail,
                        title = title,
                        ... = ...)
  } else {
    wx3:::load_file.STM(data = data,
                        worker = worker,
                        attempt = attempt,
                        threshold = threshold,
                        list_fail = list_fail,
                        title = title,
                        ... = ...)
  }
}
