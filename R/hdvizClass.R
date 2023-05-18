

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
                          credits = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)

      self$hdviz_engine <- hdviz_engine(viz)

      if(self$hdviz_engine == "ggplot"){
        formats <- unique(c(c('png', 'svg'), formats))
      }
      if(self$hdviz_engine == "htmlwidget"){
        formats <- unique(c(c('png', 'html'), formats))
      }

      if(self$hdviz_engine == "ggplot"){
        viz_meta <- get_ggplot_meta(viz)

      }

      self$name <- name
      self$description <- description
      self$slug <- slug

      self$meta$title <- meta$title %||% viz_meta$title
      self$meta$title <- meta$subtitle %||% viz_meta$subtitle
      self$meta$caption <- meta$caption %||% viz_meta$caption
      self$meta$x_lab <- meta$caption %||% viz_meta$x_lab
      self$meta$y_lab <- meta$caption %||% viz_meta$y_lab

      if(!is.null(data)){
        self$data <- viz_meta$caption
      }
      # self$hdbase <- hdbase::hdbase(self$data, name = self$name,
      #                       description = self$description,
      #                       slug = paste0(self$slug,"-slug"))

      self$width <- width
      self$height <- height

      self$hdviz_type <- hdviz_type

      if(!all(formats %in% self$available_write_formats())){
        stop("Cannot write in the format specified. Formats supported are: ",
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
        width = self$nrow,
        height = self$ncol,
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
    available_write_formats = function(hdviz_engine = NULL){
      nms <- names(self)
      nms <- nms[nms != "write_meta_json"]

      ggplot_funs <- nms[grepl("^write_ggplot_", nms)]
      ggplot_funs <- gsub("write_ggplot_","", ggplot_funs)

      htmlwidget_funs <- nms[grepl("^write_htmlwidget", nms)]
      htmlwidget_funs <- gsub("write_htmlwidget_","", htmlwidget_funs)

      if(self$hdviz_engine == "ggplot")
        return(ggplot_funs)
      if(self$hdviz_engine == "htmlwidget")
        return(htmlwidget_funs)
      NULL
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
        self[[paste0("write_",self$hdviz_engine, "_", format)]](path)
      })
    },
    write_ggplot_png = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".png"))
      ggsave(save_path)
    },
    write_ggplot_jpeg = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".jpeg"))
      ggsave(save_path)
    },
    write_ggplot_svg = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".svg"))
      ggsave(save_path)
    },
    write_ggplot_pdf = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".pdf"))
      ggsave(save_path)
    },
    write_html = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path, paste0(self$slug,".html"))
    }
  ),
  active = list(
  )
)






save_local <- function(type, viz, viz_path, path, viz_height, viz_width){
  switch(type,
         "gg" = save_ggplot(viz = viz, viz_path = viz_path,
                        viz_height = viz_height,
                        viz_width = viz_width),
         "htmlwidget" = save_htmlwidget(viz = viz,
                                        viz_path = viz_path,
                                        viz_height = viz_height,
                                        viz_width = viz_width,
                                        path = path))
}

save_ggplot <- function(viz, viz_path, viz_height, viz_width){
  ext <- turn::which_ext(viz_path)
  ggplot2::ggsave(viz_path, plot = viz,
                  width = viz_width/100, height = viz_height/100,
                  units = "in", dpi = 300,
                  device = ext)
}


save_htmlwidget <- function(viz, viz_path, viz_height, viz_width, path){
  filepath <- paste0(random_name(),".html")
  htmlwidgets::saveWidget(viz, filepath,
                          selfcontained = TRUE)
  dir.create(path, recursive = TRUE)
  file.copy(filepath, paste0(viz_path,".html"))
  # if (!webshot2::is_phantomjs_installed())
  #   webshot2::install_phantomjs()
  # webshot2::webshot(paste0(viz_path,".html"), paste0(viz_path,".png"),
  #                   vheight = viz_height, delay = 0.2)
  # file.remove(filepath)
}









