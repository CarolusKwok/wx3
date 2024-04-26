#' @title
#' Download data
#'
#' @description
#' Download data using `httr2`, with safety in mind. All the downloaded data will provide a status code of the following
#' * #FAIL: The download failed completely, and there is no successful request attempted
#' * #FINE: The download was performed, but an error code appeared or the internet connection was lost between the download attempt.
#' * #GOOD: The download is successful
#'
#' @param url (character) URL of the download file
#' @param dir (character) Directory of the downloaded file
#' @param unpw (character) Username and Password, to be passed to `httr2::req_auth_basic`
#' @param token (character) Token, to be passed to `httr2::req_auth_basic`
#'
#' @keywords internal
#' @rdname load_help
load = function(url, dir, unpw, token){
  #Preset function ####
  core = function(req, dir){
    writeBin(object = httr2::resp_body_raw(httr2::req_perform(req = req)),
             con = dir)
    return("#GOOD")
  }

  #Create request ####
  req = httr2::request(base_url = url)
  if(!rlang::is_missing(unpw)){
    if(length(unpw) == 1){
      req = httr2::req_auth_basic(req = req, username = unpw[[1]])
    } else {
      req = httr2::req_auth_basic(req = req, username = unpw[[1]], password = unpw[[2]])
    }
  }
  if(!rlang::is_missing(token)){
    req = httr2::req_auth_bearer_token(req = req, token = token)
  }

  #START TO DOWNLOAD ####
  stat = "#FAIL"
  if(wx3:::sys_hasInternet()){
    stat = tryCatch(expr = core(req = req, dir = dir),
                    error = function(e){return("#FINE")})
    if(!wx3:::sys_hasInternet()){stat = "#FAIL"}
  }
  return(stat)
}


#' @title
#' Format Dataframe
#'
#' @description
#' Provides `Exist` and `Size` columns to `load_file` and `load_fileset` functions
#'
#' @param data (data.frame) Dataframe containing columns "URL", "DIR", "Info"
#'
#' @keywords internal
#' @rdname load_help
#'
#' @examples wx3:::load_file.format(data)
load_file.format = function(data){
  return(dplyr::mutate(data,
                       Exist = file.exists(DIR),
                       Size = file.size(DIR) %>% ifelse(is.na(.), 0, .))
         )
}
