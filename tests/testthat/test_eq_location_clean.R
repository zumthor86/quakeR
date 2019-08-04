context("Cleaning LOCATION_NAME")

df <- quakeR::quakes_df %>%
  quakeR::eq_location_clean()

test_that("Location is cleaned", {

  expect_equal(sum(stringr::str_detect(df$LOCATION_NAME, "China"), na.rm = T)<30, T)
  expect_equal(sum(stringr::str_count(purrr::map_chr(df$LOCATION_NAME, ~stringr::str_sub(., 3, 3)), "A"), na.rm = T)<10, T)


})


