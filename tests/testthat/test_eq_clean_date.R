context("Cleaning NOAA data")

df <- quakeR::quakes_df %>%
  quakeR::eq_clean_data()

test_that("Has formatted DATE column", {

  expect_equal('DATE' %in% names(df), T)
  expect_equal('Date', class(df$DATE))


})

test_that("Latitude/Longitude has correct type", {

  expect_equal('numeric', class(df$LATITUDE))
  expect_equal('numeric', class(df$LONGITUDE))

})
