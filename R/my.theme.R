#' my.theme
#' created by: Benjamin Green
#' 
#' create the theme and colors objects for the density graphs
#' 
#' @return theme1 and colors in a list 
#' @export 

my.theme <- function (){
colors<-c("red3","deepskyblue3",'darkslategray4', "gold","darkorchid3",
          "darkorange1"
          , "aquamarine2",'gray48','blue')

theme1<-ggplot2::theme(
  panel.grid.major.y = ggplot2::element_blank(),
  panel.grid.major.x = ggplot2::element_blank(),
  axis.line = ggplot2::element_blank(),
  axis.title.x = ggplot2::element_text(vjust = 0, size = 6),
  axis.title.y = ggplot2::element_text(vjust = 0, size = 6),
  panel.grid.minor.y = ggplot2::element_blank(),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  plot.title = ggplot2::element_text(hjust = 0.5, size = 9),
  legend.text.align = 1,
  legend.background = ggplot2::element_blank(),
  legend.key.size = ggplot2::unit(.1,'in'),
  legend.title = ggplot2::element_text(size = 8),
  text = ggplot2::element_text(size = 8),
  panel.border = ggplot2::element_rect(
    size=.5, color='black',fill=NA),
  aspect.ratio = 3, 
  plot.margin = ggplot2::margin(
    t = 0, r = 1, b = 8, l = 1, unit = "pt"),
  legend.position = c(.8,.8)
)
out<-(list(theme1 = theme1, colors=colors))
}