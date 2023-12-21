#' @title
#' System tools: A slightly better abort messenger
#'
#' @description
#' When an error occurs, it will automatically refer to the function name that the user typed in, instead of the previous function name. Additionally, custom `cli` formating can be used to refer different stuff.
#' This function is based on `cli::cli_abort` for formatting. Note that when writing functions, **do not** use pipe (`%>%`, `|>`) before this function.
#'
#' Supports inline-markup themes. For more information on those custom themes, please refer to `wx3:::sys_msg_theme` and `cli::cli_div`.
#'
#' @param message Message to be displayed, formatted via `cli::cli_bullets()`. For more information, please read `cli::cli_abort`.
#' @param x An argument name in the current function.
#' @param arg Argument to be traced. When referring to this in argument `message`, use `{.arg {arg}}`.
#' @param ... Additional items to consider in the message. Any items that uses glue (`{}`) must be passed through here.
#'
#' @return An abort message
#' @keywords internal
#'
#' @examples wx3:::sys_abort(message = "test")
sys_abort = function(message, x, arg = rlang::caller_arg(x), ...){
  #1 Get themes ####
  wx3:::sys_msg_theme()

  #2 Pass dot to the environment ####
  list2env(x = list(...), envir = rlang::current_env())

  #3 Message ####
  n = -1
  list = list()
  while(TRUE){
    n = n + 1
    call = rlang::caller_env(n = n)
    list = append(list, values = call)
    if(identical(call, y = globalenv())){
      break}
  }
  list = rev(list)
  for(i in seq_along(list)){
    if(identical(x = list[[i]], y = globalenv())){next}
    if(identical(x = ls(envir = list[[i]]), y = character(0L))){next}
    cli::cli_abort(message = message, call = list[[i]])
  }
}
