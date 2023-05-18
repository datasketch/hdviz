

#' @title Create a hdviz object
#' @description Create a hdtable object from a data frame. The main value of a hdtable is its metadata. When creating it, hdtable will add to the data frame the following information:
#'
#' - viz: visualization object (ggplot or htmlwidget)
#' - name: Name for the hdviz, setted on _name_ argument
#' - description: Description for the hdviz, setted on _description_ argument
#' - slug: a custom slug can be added
#' @param x A data frame
#' @param hdtable_type The type of hdtable to create
#' @param name a custom name can be added
#' @param description a custom description can be added
#' @param slug a custom slug can be added. If not, hdtable will try creating one.
#' @param meta Custom Metadata can be added
#'
#' @examples
#' hdtable(mtcars, hdtable_type = "Num", name = "MTCars")
#'
#' @return A hdtable object
#' @export
hdviz <- function(viz,
                  name = NULL,
                  description = NULL,
                  slug = NULL,
                  meta = NULL,
                  width = NULL,
                  height = NULL,
                  formats = NULL,
                  ...){

  if(is_hdviz(d)) return(d)

  name <- name %||% deparse(substitute(d))
  meta <- c(meta, list(...))
  if(dstools::is.empty(meta)) meta <- NULL

  hdvizClass$new(viz,
                 name = name, description = description,
                 slug = slug, meta = meta, formats = formats)
}






#' @title is_hdviz
#' @description test for objects of type "hdviz"
#'
#' @param x object to be coerced or tested
#'
#' @return returns TRUE or FALSE depending on whether its argument is of type hdviz or not.
#'
#' @examples
#' some_viz <- hdviz(viz)
#' is_hdviz(some_viz)
#'
#' @export
is_hdviz <- function(x) {
  inherits(x, "hdviz")
}





#' @export
hdviz_update_meta <- function(f, ...){
  fixed <- c("viz", "hdviz_type")
  args <- list(...)
  if(any(names(args) %in% fixed)){
    warning("Cannot update ",
            paste0(names(args)[names(args) %in% fixed], collapse = ", "),
            ". Removing from meta.")
    args <- args[!names(args) %in% fixed]
  }

  f$name <- args$name %||% f$name
  f$description <- args$description %||% f$description
  f$slug <- args$slug %||% f$slug
  meta <- args[!names(args) %in% c("name", "description","slug")]
  common_names <- intersect(names(f$meta), names(meta))
  # Delete info from common names
  purrr::walk(common_names, function(nm){
    message(nm)
    f$meta[[nm]] <- NULL
  })
  updated_meta <- purrr::list_modify(list(f$meta), meta)[[1]]
  f$meta <- updated_meta
  f
}










#' @export
hdviz_engine <- function(viz){
  if(any(c("gg", "ggmagic") %in% class(viz))){
    return("ggplot")
  }
  if(all(c("htmlwidget") %in% class(viz))){
    return("htmlwidget")
  }
}





