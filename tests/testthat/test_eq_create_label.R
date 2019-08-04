context("Label column created")

label_col <- quakeR::quakes_df %>%
  quakeR::eq_create_label()

test_that("Returns character vector", {

  expect_equal(class(label_col), "character")

})

