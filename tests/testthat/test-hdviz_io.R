test_that("hdviz io works", {


  library(ggplot2)
  gg <- ggplot(cars, aes(speed, dist)) + geom_point()

  hv <- hdvizClass$new(gg, name = "Chart")

  path <- "tmp/chart"
  hv$write_ggplot_png(path)
  hdviz_write(hv, "tmp")

  expected_write_ext <- c(".meta.json",".png", ".rds", ".svg")
  expect_equal(list.files(file.path("tmp",hv$slug)),
               paste0(hv$slug, expected_write_ext))
  unlink(file.path("tmp",hv$slug), recursive = TRUE)

})
