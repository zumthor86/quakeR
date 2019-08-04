#' Clean location column of NOAA earthquake dataset
#'
#' @param quakes_df A dataframe with the same column specification as NOAA earthquake data
#'
#' @examples
#' \dontrun{
#' cleaned_df <- eq_location_clean(quakes_df)
#' }
#'
#' @return Dataframe containing a LOCATION column with the country stripped out
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom purrr modify_at
#' @export
eq_location_clean <- function(quakes_df){

  quakes_df %>%
    purrr::modify_at('LOCATION_NAME', ~stringr::str_remove(.,pattern =  "\\w*: ")) %>%
    purrr::modify_at('LOCATION_NAME', stringr::str_to_title)

}
