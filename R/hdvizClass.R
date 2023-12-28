

hdvizClass <- R6::R6Class(
  "hdviz",
  public = list(
    name = NULL,
    slug = NULL,
    description = NULL,
    formats = NULL,
    viz = NULL,
    width = NULL,
    height = NULL,
    hdviz_type = NULL,
    hdviz_engine = NULL,
    meta = NULL,
    data = NULL,
    hdbase = NULL,
    credits = NULL,

    initialize = function(viz, hdviz_type = NULL,
                          hdviz_engine = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, meta = NULL,
                          formats =  NULL,
                          data = NULL,
                          width = NULL, height = NULL,
                          credits = NULL,
                          slug_append_random = FALSE) {

      name <- name %||% deparse(substitute(viz))
      description <- description %||% ""
      slug <- slug %||%
        dstools::create_slug(name, append_random = slug_append_random)

      self$hdviz_engine <- hdviz_engine(viz)

      if(self$hdviz_engine == "ggplot"){
        formats <- unique(c(c('png', 'svg'), formats))
      }
      if(self$hdviz_engine == "htmlwidget"){
        formats <- unique(c(c("html", "png"), formats))
      }

      if(self$hdviz_engine == "ggplot"){
        viz_meta <- get_ggplot_meta(viz)
      }
      if(self$hdviz_engine == "htmlwidget"){
        viz_meta <- list()
      }

      self$name <- name
      self$description <- description
      self$slug <- slug
      self$viz <- viz

      self$meta$title <- meta$title %||% viz_meta$title
      self$meta$subtitle <- meta$subtitle %||% viz_meta$subtitle
      self$meta$caption <- meta$caption %||% viz_meta$caption
      self$meta$x_lab <- meta$caption %||% viz_meta$x_lab
      self$meta$y_lab <- meta$caption %||% viz_meta$y_lab

      if(!is.null(data)){
        self$data <- viz_meta$data
      }
      # self$hdbase <- hdbase::hdbase(self$data, name = self$name,
      #                       description = self$description,
      #                       slug = paste0(self$slug,"-slug"))

      self$width <- width %||% 800
      self$height <- height %||% 800

      self$hdviz_type <- hdviz_type

      if(!all(formats %in% self$available_write_formats(self$hdviz_engine))){
        stop("Formats supported are: ",
             paste(self$available_write_formats(), collpase = ", "))
      }
      self$formats <- formats
      self$meta <- meta


      self$credits <- "Visualization hosted at http://datasketch.co"


    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        width = self$width,
        height = self$height,
        credits = self$credits
      )
      c(base_info, self$meta)
    },
    write_meta_json = function(path = "", overwrite_dic = FALSE){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path,paste0(self$slug,".meta.json"))
      metadata <- self$metadata()
      metadata$hdviz_type <- as.character(metadata$hdviz_type)
      jsonlite::write_json(metadata, save_path,
                           auto_unbox = TRUE, pretty = TRUE)
    },
    available_write_formats = function(hdviz_engine){
      nms <- names(self)
      nms <- nms[nms != "write_meta_json"]

      ggplot_funs <- nms[grepl("^write_ggplot_", nms)]
      ggplot_funs <- gsub("write_ggplot_","", ggplot_funs)

      htmlwidget_funs <- nms[grepl("^write_htmlwidget_", nms)]
      htmlwidget_funs <- gsub("write_htmlwidget_","", htmlwidget_funs)

      if(self$hdviz_engine == "ggplot")
        return(ggplot_funs)
      if(self$hdviz_engine == "htmlwidget")
        return(htmlwidget_funs)
    },
    write_rds = function(path = ""){
      save_path <- file.path(path,paste0(self$slug,".rds"))
      # Write RDS
      saveRDS(self$viz, save_path)
    },
    write = function(path = ""){
      path <- file.path(path, self$slug)
      # Write metadata
      self$write_meta_json(path)
      # Write RDS
      self$write_rds(path)
      # Write formats
      purrr::walk(self$formats, function(format){
        fun <- paste0("write_",self$hdviz_engine,"_",format)
        message("Writing format:", format, fun)
        self[[fun]](path)
      })
    },
    write_ggplot_png = function(path = "."){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".png"))
      save_ggplot(self$viz, save_path)
    },
    write_ggplot_jpeg = function(path = "."){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".jpeg"))
      save_ggplot(self$viz, save_path)
    },
    write_ggplot_svg = function(path = "."){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".svg"))
      save_ggplot(self$viz, save_path)
    },
    write_ggplot_pdf = function(path = "."){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".pdf"))
      save_ggplot(self$viz, save_path)
    },
    write_htmlwidget_html = function(path = "."){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".html"))
      #htmlwidgets::saveWidget()
      save_htmlwidget(self$viz, save_path,
                      viz_width = self$width,
                      viz_height = self$height)
    },
    write_htmlwidget_png = function(path = "."){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path_html <- file.path(path, paste0(self$slug,".html"))
      save_path_png <- file.path(path, paste0(self$slug,".png"))
      x <- webshot2::webshot(save_path_html, vwidth = self$width,
                             vheight = self$height,
                             cliprect = "viewport",
                             delay = 0.3)
      file.rename(as.character(x), save_path_png)
    }
  ),
  active = list(
  )
)


save_ggplot <- function(viz, viz_path, viz_width = NULL, viz_height = NULL){
  ext <- which_ext(viz_path)
  viz_width <- viz_width %||% 10
  viz_height <- viz_height %||% 7.5
  ggplot2::ggsave(viz_path, plot = viz,
                  width = viz_width, height = viz_height,
                  units = "cm", dpi = 300,
                  device = ext,
                  bg = NULL)
}


save_htmlwidget <- function(viz, save_path, viz_width = NULL, viz_height = NULL){
  filepath <- paste0(dstools::random_name(),".html")
  str(viz)
  str(filepath)

  htmlwidgets::saveWidget(viz, filepath, selfcontained = TRUE)
  viz_width <- viz_width %||% 800
  viz_height <- viz_height %||% 800

  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  file.rename(filepath, save_path)

}









