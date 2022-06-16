##' create.clean.xy
#' 
#' create by : benjamin green
#' 
#' create a clean xy plot from provided data frame with variables named x and y
#' 
#' 
create.clean.xy <- function(my.graph.d1, cols, 
                            my.ylab= '',my.xlab= '',my.title= '',
                            regression = FALSE){
  theme1 <- my.theme()
  theme1 <- theme1$theme1
#
#
#
  if (regression == TRUE) {
    myline =  ggpubr::stat_cor(
      label.x = max.x * .15, label.y = max.y * 1.35
    ) +
      ggpubr::stat_regline_equation(
        label.x = max.x * .15, label.y = max.y * 1.23
      )
  } else {
    myline = ggplot2::theme()
  }
  #
  #
  #
  ggpubr::ggscatter(
    my.graph.d1, 'x', 'y', color = cols, size = .7,
    add = "reg.line",
    add.params = list(color = "black", fill = 'lightgrey'), 
    conf.int = TRUE,
    ylab = my.ylab,
    xlab = my.xlab,
    title = my.title,
    ggtheme = theme1) +
    myline +
    ggplot2::theme(
      aspect.ratio = 1, 
      legend.position = c(.2, .9),
      plot.margin = ggplot2::margin(
        t = 10, r = 10, b = 10, l = 10, unit = "pt"),
      axis.title.x = ggplot2::element_text(
        vjust = 0, size = 10),
      axis.text.x=ggplot2::element_text(
        vjust = 0, size = 9),
      axis.title.y = ggplot2::element_text(
        vjust = 0, size = 10),
      axis.text.y=ggplot2::element_text(
        vjust = 0, size = 9),
      axis.ticks.x=ggplot2::element_line()
    )
  }