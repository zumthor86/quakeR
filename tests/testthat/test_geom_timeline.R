context("geom_timeline adds layer to ggplot")

plot <- quakeR::quakes_df %>%
  dplyr::filter(YEAR>2000) %>%
  eq_clean_data() %>%
  eq_location_clean() %>%
  dplyr::filter(COUNTRY %in% c("CHINA", "TURKEY", "INDIA", "RUSSIA"))  %>%
  ggplot2::ggplot(aes(x=DATE,size=EQ_PRIMARY,y=COUNTRY, color=DEATHS, label=LOCATION_NAME))+
  geom_timeline()

test_that("Class is ggplot", {

  expect_equal("ggplot" %in% class(plot), T)

})

test_that("geom class is GeomTimeline", {

  expect_equal("GeomTimeline" %in% class(plot$layers[[1]]$geom), T)

})
