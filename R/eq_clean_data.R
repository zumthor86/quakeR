#' Clean NOAA dataframe
#'
#' @param quakes_df A dataframe with the same column specification as NOAA data
#'
#' @return A modified dataframe containing a 'DATE' column and numeric 'LATITUDE' and 'LONGITUDE' columns
#'
#' @note Dates with missing month and day will have these missing values set to 1, and observations with BC dates are filtered.
#'
#' @importFrom rlang .data
#' @importFrom purrr modify_at
#' @importFrom tidyr replace_na
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#'
#' @examples
#' \dontrun{
#' cleaned_df <- eq_clean_data(raw_NOAA_df)
#' }
#' @export
eq_clean_data <- function(quakes_df){

  quakes_df %>%
    purrr::modify_at(c("MONTH", "DAY"), ~ tidyr::replace_na(., 1)) %>%
    dplyr::filter(.data$YEAR>0) %>%
    tidyr::unite("DATE", c("YEAR", "MONTH", "DAY"), sep="-") %>%
    purrr::modify_at("DATE", as.Date) %>%
    purrr::modify_at(c("LATITUDE", "LONGITUDE"), as.numeric)

}
