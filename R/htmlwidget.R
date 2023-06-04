
get_htmlwidget_meta <- function(viz){

  viz_meta <- list()
  if(!hdviz_engine(gg) == "htmlwidget"){
    stop("Not an htmlwidget object")
  }
  if("highchart" %in% class(viz)){
    return(viz$x$hc_opts$title$text)
  }

  if("leaflet" %in% class(viz)){
  }

}

