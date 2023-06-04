test_that("hdviz works", {

  # Ggplot

  library(ggplot2)

  gg <- ggplot(cars, aes(speed, dist)) + geom_point()

  hv <- hdvizClass$new(gg, name = "Chart", data = cars)

  expect_true(is_hdviz(hv))

  h2 <- hdviz(hv)
  expect_equal(hv, h2)

  hv <- hdviz_update_meta(hv, name = "CHART")
  expect_equal(hv$name, "CHART")

  ## Htmlwidget

  library(leaflet)

  lt <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng= -74.1, lat= 4.66, popup="Bog")
  lt
  viz <- lt
  hdviz <- hdviz(lt, slug = "map1", width = 2000, height = 2000)

  path <- "tmp/map3"
  #hdviz$write_htmlwidget_html(path)

  hdviz_write(hdviz,path)


})
