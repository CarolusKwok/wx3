#' System tools: Format data with exist and size column in any sys_load_... functions
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#'
#' @keywords internal
#' @noRd
#'
#' @examples wx3:::load_file.format(data)
load_file.format = function(data){
  return(dplyr::mutate(data,
                       Exist = file.exists(DIR),
                       Size = file.size(DIR) %>% ifelse(is.na(.), 0, .))
         )
}
