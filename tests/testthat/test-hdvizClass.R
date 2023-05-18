test_that("Test hdvizClass", {

  library(ggplot2)

  gg <- ggplot(cars, aes(speed, dist)) + geom_point()

  hv <- hdvizClass$new(gg, name = "Chart")

  expect_equal(hv$hdviz_engine, "ggplot")
  expect_equal(hv$available_write_formats(), c("pdf", "svg", "jpeg", "png"))

  path <- "tmp/hdb"
  hv$write_meta_json(path)
  hv$write_rds(path)
  hv$write_ggplot_png(path)

  hv$write(path)


})

test_that("Hdviz",{



  gg <- ggplot() + geom_point()

  expect_equal(hdviz_engine(gg), "ggplot")



})


