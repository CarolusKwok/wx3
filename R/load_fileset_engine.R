#' @title System tool: Engine of the load_fileset function
#'
#' @description
#' The engine to download filesets from a site, using parallel computation.
#'
#' @param data_download (data.frame) Data to be downloaded, refer to `data` in `wx3::load_fileset`
#' @param tmp_dir (character) Temporary directory of the main `R` session
#' @param threshold (numeric) The minimum file size in bytes, similar to `threshold` in `wx3::load_fileset`
#' @param ... Additional parameters to be passed to `RCurl::getBinaryURL`
#'
#' @return NA
#' @keywords internal
load_fileset.engine = function(data_download, tmp_dir, threshold, ...){
  tmp_file = list.files(tmp_dir, full.names = TRUE)
  file.remove(tmp_file)

  list_DIR = unique(data_download$DIR)
  control = rep(x = TRUE, length(list_DIR))
  env = environment()

  data_download = dplyr::mutate(.data = data_download,
                                index = dplyr::row_number(),
                                DIR_index = match(x = DIR, table = list_DIR),
                                TMP = paste0(tmp_dir, "/", index, "_", basename(DIR)))


  progressr::handlers(progressr::handler_cli(format = stringr::str_flatten(c("{cli::pb_spin} ", length(list_DIR), " fileset {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"))))

  nrow = nrow(data_download)
  steps = round(x = nrow, digits = -floor(log10(nrow)))/20

  progressr::with_progress({
    p = progressr::progressor(steps = nrow/steps, auto_finish = FALSE, initiate = TRUE)
    stat = furrr::future_map_chr(.x = seq_len(nrow(data_download)),
                                 .f =
                                   function(.x, steps, data_download, threshold, env, ...){
                                     if(round(.x %% steps) == 1){p()}
                                     status = "#NA"

                                     DIR_index = data_download$DIR_index[[.x]]
                                     control = eval(expr = parse(text = paste0("control[[", DIR_index, "]]")),
                                                    envir = env)
                                     if(control){
                                       tmp_x = data_download$TMP[[.x]]
                                       url_x = data_download$URL[[.x]]
                                       status = wx3:::load(url = url_x, dir = tmp_x, ...)

                                       if(status == "#FAIL"){
                                         return(status)
                                       } else if(status == "#GOOD" & file.size(tmp_x) >= threshold) {
                                         eval(expr = parse(text = paste0("control[[", DIR_index, "]] = FALSE")),
                                              envir = env)
                                       } else {
                                         status = "#NA"
                                       }
                                     }
                                     return(status)},
                                 steps = steps, data_download = data_download, threshold = threshold, env = env, ... = ...)
  })

  if("#FAIL" %in% stat){
    call::warn(message = c("x" = "Error: Download incomplete",
                           "i" = "Error due to network failure at any point of this attempt",
                           "i" = "No files are created"))
  } else if("#GOOD" %in% stat) {
    TMP_DIR = dplyr::mutate(.data = data_download,
                            status = stat) %>%
      dplyr::filter(status == "#GOOD") %>%
      dplyr::group_by(DIR_index) %>%
      dplyr::filter(DIR_index == min(DIR_index)) %>%
      dplyr::ungroup()
    if(nrow(TMP_DIR) >= 1){
      for(X in unique(dirname(TMP_DIR$DIR))){dir.create(path = X, showWarnings = FALSE, recursive = TRUE)}
      file.rename(from = TMP_DIR$TMP, to = TMP_DIR$DIR)
    }
  }

  tmp_file = list.files(tmp_dir, full.names = TRUE)
  file.remove(tmp_file)
}
