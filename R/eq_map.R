#' Create map of quakes
#'
#' @param df A dataframe containing at a minimum, earthquake magnitude, latitude and longitude and a column to label the markers with.
#' @param annot_col Name of column containing desired marker label
#'
#' @return Leaflet map
#' @export
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom assertthat::assert_that
#'
#' @examples
#' \dontrun{
#' quakes_df %>%
#'  eq_clean_data() %>%
#'  eq_location_clean() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
#'
#'
eq_map <- function(df, annot_col='popup_text'){

  assertthat::assert_that(is.data.frame(df),msg =  "Input to eq_map must be a dataframe")

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = df[['LONGITUDE']],
      lat = df[['LATITUDE']],
      radius = df[['EQ_PRIMARY']], popup = df[[annot_col]])

}
