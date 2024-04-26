#' @title
#' Internet check
#'
#' @description
#' Test if there is internet access, based on network connect with websites. By default, the list of websites to be tested can be found in `sys_hasInternet.txt` in the package.
#'
#' By default, the following website will be used for testing
#' * google.com
#' * bing.com
#' * un.org/zh
#'
#' Other website can also be used for testing, by inputing the address into the `test_host`
#'
#' @param test_host (character) Array of website to be tested for testing internet connection. Accepts following value.
#' * NULL: Default list of website to be tested, which can be modified in `extdata`
#' * Any other character: Website URL/ IP addresses.
#' For most purposes, please input the URL instead of IP address, as testing DNS server connectivity is also practical for most purposes.
#' @param attempt (integer) Attempts to be made per website listed in `test_host`
#'
#' @return logical value
#' * TRUE: Has internet connection
#' * FALSE: No internet connection
#' @keywords internal
#'
#' @examples sys_hasInternet()
sys_hasInternet = function(test_host = NULL,
                           attempt = 5L){
  if(is.null(test_host)){
    test_host = trimws(stringr::str_split_1(readLines(con = system.file("extdata", "sys_hasInternet.txt", package = "wx3")),
                                            pattern = ";"))
  }

  attempts = expand.grid(host = test_host,
                         attempt = seq_len(length(attempt)),
                         stringsAsFactors = FALSE)
  for(i in seq_len(nrow(attempts))){
    stat = tryCatch(expr = is.character(curl::nslookup(host = attempts$host[[i]])),
                    error = function(e){FALSE})
    if(stat){break}
  }
  return(stat)
}
