

get_ggplot_meta <- function(gg){

  if(!hdviz_engine(gg) == "ggplot"){
    stop("Not a ggplot object")
  }

  gb <- ggplot_build(gg)

  d <- gb$plot$data

  title <- gb$plot$labels$title
  subtitle <- gb$plot$labels$subtitle
  x_lab <- gb$plot$labels$x
  y_lab <- gb$plot$labels$y

  list(
    title = title,
    subtitle = subtitle,
    x_lab = x_lab,
    y_lab = y_lab,
    data = d
  )


}

