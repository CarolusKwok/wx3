#' System tool: Return system info
#'
#' @return A list of system information
#' @keywords internal
#'
#' @examples wx3::sys_info()
sys_info = function(){
  return(list(nodename = Sys.info()[["nodename"]],
              user = Sys.info()[["user"]],
              pid = Sys.getpid(),
              time = Sys.time()))
}
