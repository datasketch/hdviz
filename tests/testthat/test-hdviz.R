test_that("hdviz works", {

  library(ggplot2)

  gg <- ggplot(cars, aes(speed, dist)) + geom_point()

  hv <- hdvizClass$new(gg, name = "Chart", data = cars)

  expect_true(is_hdviz(hv))

  h2 <- hdviz(hv)
  expect_equal(hv, h2)

  hv <- hdviz_update_meta(hv, name = "CHART")
  expect_equal(hv$name, "CHART")




})
