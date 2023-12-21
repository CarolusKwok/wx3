#' System tools: Print out the download initiation panel
#'
#' @param title Title of the downloaded items
#' @param time_start Start of time
#' @param data_1 The first item to be downloaded
#' @param data_n The last item to be downloaded
#' @param data_num Number of items to download
#'
#' @keywords internal
#' @noRd
#'
#' @examples sys_ldhp_panelstart("test", Sys.time(), "1", "n", 999)
load_panelstart = function(title, time_start, data_1, data_n, data_num, storm = F){

  if(storm){
    d = cli::cli_div(theme = list(rule = list("color" = "black",
                                              "font-weight" = "bold",
                                              "background-color" = "yellow",
                                              "line-type" = 1)))
    cli::cli_rule(center = "STORM")
    cli::cli_end(d)
    }
  d = cli::cli_div(theme = list(rule = list("color" = "green",
                                            "font-weight" = "bold",
                                            "line-type" = 2)))
  cli::cli_rule(center = "initiate download")
  cli::cli_end(d)

  cli::cli_alert_info("Info")
  cli::cli_alert("Downloading {.var {title}}")
  cli::cli_alert("Start Time: {round(time_start)}")
  cli::cli_alert("First Data: {data_1}")
  cli::cli_alert("Last  Data: {data_n}")
  cli::cli_alert("    Number: {data_num}")

  d = cli::cli_div(theme = list(rule = list("color" = "grey",
                                            "font-weight" = "bold",
                                            "line-type" = 2)))
  cli::cli_rule()
  cli::cli_end(d)
}


#' System tools: Print out the download complete panel
#'
#' @param time_start Time of the download process starts
#' @param time_end Time of the download process ends
#' @param attempts Total attempts made to download all the items
#' @param success Total number of success
#' @param fail Total number of fails
#' @param list_fail Should you list the failed-to-download items?
#' @param list_of_fail The list of failed to download items.
#' @param length The length of the panel to be drawn. Default as `51` characters
#'
#' @keywords internal
#' @noRd
#'
#' @examples sys_ldhp_panelend(Sys.time(), Sys.time(), 999, 0, 9, TRUE, "fail")
load_panelend = function(time_start, time_end, attempts, success, fail, list_fail, list_of_fail, length = 51){
  time_diff = round(as.numeric(difftime(time_end, time_start, units = "mins")), digits = 3)

  d = cli::cli_div(theme = list(rule = list("color" = "red",
                                            "font-weight" = "bold",
                                            "line-type" = 1)))
  cli::cli_rule(center = "download complete")
  cli::cli_end(d)

  cli::cli_alert_info("Info")
  cli::cli_alert("End Time: {round(time_end)}")
  cli::cli_alert("Eclipsed: {time_diff} mins")
  cli::cli_alert(" Attempt: {stringr::str_flatten(attempts, collapse = ',')}")
  cli::cli_alert(" Success: {success}")
  cli::cli_alert("    Fail: {fail}")
  if(list_fail){
    num = length(list_of_fail)
    if(num > 0){
      cli::cli_alert_danger("Failed items as follow:")
      cli::cli_bullets(c(" " = "{stringr::str_flatten(list_of_fail, collapse = ", ")}"))
    } else {
      cli::cli_alert_success("There are no failed items!")
    }
  }
  cli::cli_text("")
}
