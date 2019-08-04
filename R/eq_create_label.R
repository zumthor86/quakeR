#' Create informative popup label from quakes dataframe
#'
#' @param df NOAA quake dataframe containing LOCATION_NAME, EQ_PRIMARY, TOTAL_DEATHS columns
#'
#' @return Character vector of HTML
#' @export
#'
#' @examples
#' \dontfun{
#'
#'  quakes_df$popup_text = eq_create_label(quakes_df))
#'
#' }
#'
#'
eq_create_label <- function(df){

  paste0("<b>Location</b>: ",
         df[['LOCATION_NAME']],
         '<br>',
         "<b>Magnitude</b>: ",
         df[['EQ_PRIMARY']],
         '<br>',
         "<b>Total Deaths</b>: ",
         df[['TOTAL_DEATHS']])

}
