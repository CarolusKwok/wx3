#' @title
#' Calculate by smoothing and filling the columns, with linear models
#'
#' @description
#' Data smoothing is an important process in data analysis to fill up missing data, or to remove extreme values.
#' This function fills up missing data with linear model, predicted by the head and tail of the gap.
#'
#' @param data (data.frame) a *ungrouped* data.frame storing all observations
#' @param based The column name of how the data should be arranged, in ascending order. This column must not contain NAs, must be unique, and must be "numericable".
#' @param value The column name of what values should be smoothed. This column must contain NAs.
#' @param max_fill (numeric) The maximum size of a NA-gap or __special value__ `NULL`, which ignores this argument.
#' @param trailing (logical) Remove all NAs at the head of prediction column? Default as `TRUE`.
#' @param name_as (character) Name of the new columns, the following are supported.
#' * `NULL`: Create a new column as column name of `value` with prefix `slm_`
#' * `*del*`: Replaces the original column with the smoothed column
#' * Any other character: Name of the column. May overwrite the original dataframe columns if `overwrite` == TRUE
#' @param overwrite (logical) Let the new column names to overwrite the original dataframe columns?
#' * FALSE: Do not overwrite
#' * TRUE: Do overwrite
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_smooth_lm(data, x, y, trailing = F)
calc_smooth_lm = function(data,
                          based,
                          value,
                          max_fill = NULL,
                          trailing = TRUE,
                          name_as = NULL,
                          overwrite = FALSE){
  #Arrange data so its in ascending based order ####
  data = dplyr::arrange(data, {{based}})
  #Find the data used, get NA values ####
  data0 = dplyr::select(data,
                        x = {{based}},
                        y = {{value}}) %>%
    dplyr::mutate(row = 1:dplyr::n(),
                  NAs = is.na(y))

  #Find the group ####
  ls_grp = dplyr::filter(data0, NAs)$row
  ls_grp = sort(unique(c(ls_grp, (ls_grp+1), (ls_grp-1))))
  df_grp = dplyr::filter(data0,
                         row %in% ls_grp) %>%
    dplyr::bind_rows(.,.) %>%
    dplyr::arrange(x) %>%
    dplyr::filter(!NAs) %>%
    dplyr::slice(2:(dplyr::n()-1)) %>%
    dplyr::mutate(groups = as.factor(ceiling((1:dplyr::n())/2)))

  #Left join data ####
  df_grp1_sel = dplyr::group_by(df_grp, x) %>%
    dplyr::reframe(grp1 = as.factor(min(as.numeric(groups), na.rm = T)))
  df_grp2_sel = dplyr::group_by(df_grp, x) %>%
    dplyr::reframe(grp2 = as.factor(max(as.numeric(groups), na.rm = T)))
  df_grp_sel = dplyr::rename(df_grp1_sel, grp = grp1)

  data0 = dplyr::left_join(x = data0, y = dplyr::select(.data = df_grp_sel, x, grp), by = "x") %>%
    dplyr::left_join(y = dplyr::select(.data = df_grp1_sel, x, grp1), by = "x") %>%
    dplyr::left_join(y = dplyr::select(.data = df_grp2_sel, x, grp2), by = "x") %>%
    tidyr::fill(grp, .direction = "updown") %>%
    tidyr::fill(grp1, .direction = "down") %>%
    tidyr::fill(grp2, .direction = "up")

  #Form the model ####
  df_grp = dplyr::rename(df_grp, grp = groups)
  max_grp = max(as.numeric(df_grp$grp), na.rm = T)
  if(max_grp == 1){
    lm = lm(formula = y~x, data = df_grp, singular.ok = T)
    prediction = suppressWarnings(predict(object = lm, newdata = data0))
  } else {
    lm = lm(formula = y~x * grp, data = df_grp, singular.ok = T)
    prediction = suppressWarnings(predict(object = lm, newdata = data0))
  }
  data0 = dplyr::mutate(data0,
                        predict = ifelse(!NAs, y, prediction))

  #Replace with NA value if trailing == T ####
  if(trailing){
    data0 = dplyr::mutate(data0,
                          predict = ifelse(is.na(grp1) & NAs, NA, predict),
                          predict = ifelse(is.na(grp2) & NAs, NA, predict))
  }
  #Replace with NA value if > group size ####
  if(!is.null(max_fill)){
    df_grp_size = dplyr::group_by(.data = df_grp, grp) %>%
      dplyr::reframe(size = max(x) - min(x))
    ## Find faulty groups ####
    ls_grpX = dplyr::filter(df_grp_size,
                            size > max_fill)$grp
    ## Replace! ####
    data0 = dplyr::mutate(data0,
                          predict = ifelse(grp %in% ls_grpX, NA, predict))
  }
  #Replace with known value ####
  data0 = dplyr::mutate(data0, predict = ifelse(NAs, predict, y))

  #Return the data ####
  return(data0$predict)
}
