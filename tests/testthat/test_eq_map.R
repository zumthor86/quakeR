context("eq_map returns leaflet map")

leaf <-  quakeR::quakes_df %>%
  quakeR::eq_clean_data() %>%
  quakeR::eq_location_clean() %>%
  head(10) %>%
  dplyr::mutate(popup_text = quakeR::eq_create_label(.)) %>%
  quakeR::eq_map(annot_col = "popup_text")

test_that("Class is leaflet", {

            expect_equal("leaflet" %in% class(leaf), T)

          })

test_that("Error is thrown if input not a df", {

  expect_error(eq_map(c(1,2,3)), "Input")

})
