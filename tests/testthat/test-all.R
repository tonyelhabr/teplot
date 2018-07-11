
context("maps_tx")
require("ggplot2")
require("maps")

test_that("maps_tx", {

  expect_equal(convert_state_abb_to_name("tx"), "texas")

  map_data_county_tx <- get_map_data_county("tx")
  expect_equal(nrow(map_data_county_tx), 4488)
  expect_equal(ncol(map_data_county_tx), 6)
  expect_equal(map_data_county_tx, get_map_data_county_tx())

  map_data_state_tx <- get_map_data_state("tx")
  expect_equal(nrow(map_data_state_tx), 1088)
  expect_equal(ncol(map_data_state_tx), 6)
  expect_equal(map_data_state_tx, get_map_data_state_tx())

  expect_equal(get_color_inv("#FFFFFF"), "#000000")

  # NOTE: This test is throwing an error for some reason...
  map_tx <- create_map_state(state = "texas")
  expect_true(ggplot2::is.ggplot(map_tx))

  map_base <- create_map_base(state = "texas")
  expect_true(ggplot2::is.ggplot(map_base))

  map_base_tx <- create_map_base_tx()
  expect_true(ggplot2::is.ggplot(map_base_tx))


})

context("theme")

test_that("theme", {

  theme_te <- theme_te()
  expect_true(ggplot2::is.theme(theme_te))
  expect_true(length(theme_te) == 59)

  theme_te_dx <- theme_te_dx()
  expect_true(!is.null(theme_te_dx$panel.grid.major.x))
  expect_equal(length(theme_te), length(theme_te_dx) - 1)

  theme_te_facet <- theme_te_facet()
  expect_true(!is.null(theme_te_facet$panel.background))
  expect_equal(length(theme_te), length(theme_te_facet))

  theme_te_facet_dx <- theme_te_facet_dx()
  expect_equal(length(theme_te), length(theme_te_facet_dx) - 1)

  theme_map <- theme_map()
  expect_equal(length(theme_te), length(theme_map))

})
