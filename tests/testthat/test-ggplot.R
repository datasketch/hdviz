test_that("ggplot metadata", {

  library(ggplot2)

  d <- cars
  gg <- ggplot(d, aes(speed, dist)) + geom_point() +
    labs(
      title = "My chart",
      subtitle = "This is a subtitle"
    )
  l <- get_ggplot_meta(gg)
  expect_equal(l$title, "My chart")
  expect_equal(l$subtitle, "This is a subtitle")
  expect_equal(l$data, d)


})
