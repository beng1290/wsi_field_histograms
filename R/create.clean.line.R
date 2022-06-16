##' create.clean.line
#' 
#' create by : benjamin green
#' 
#' create a clean line plot from provided data frame with variables named x and y
#' 
#' 
create.clean.line <- function(
  my.graph.d1, drawme = TRUE, 
  my.ylab= '',my.xlab= '',my.title= '',
  m.aspect.ratio = 1, z, m.legend = FALSE,
  legend.title = '', logl = 0
){
  #
  # graph theme
  #
  theme1 <- my.theme()
  cols <- theme1$colors
  theme1 <- theme1$theme1
  #
  # colors and legends
  #
  stat.type <- unique(my.graph.d1$sampleid)
  stat.typeb <- stat.type
  #
  if (length(stat.type) == 4){
      stat.type <- sort(stat.type)
      v <- dplyr::summarise(
        dplyr::group_by(my.graph.d1, sampleid),
        cc = max(c), .groups = 'drop'
        )
      cc <- v$cc
      names(cc) <- v$sampleid
      cc<- cc[order(names(cc))]
      stat.typeb <- c(
        paste0('NR large (n = ', cc[1],')'),
        paste0('NR small (n = ', cc[2],')'),
        paste0('R large (n = ', cc[3],')'),
        paste0('R small (n = ', cc[4],')')
      )
  }
  #
  if (length(stat.type) < length(cols)){
    my.col.vals <- cols[1:length(stat.type)]
    names(my.col.vals)<-stat.type
    colo = ggplot2::scale_color_manual(
      values =  my.col.vals, breaks = stat.type, labels = stat.typeb
      ) 
  } else{
    colo = ggplot2::theme()
  }
  #
  # plot a zoomed version
  #
  if (z== 'zoomed'){
    if(max(my.graph.d1$x) > 1){
      s <- easy.x.scales(40)
    } else {
      s <- easy.x.scales(.5)
    }
  } else {
    s <- easy.x.scales(my.graph.d1$x)
  }
  #
  # plot on log scale or set normal scaling to start at zero
  #
  if (logl == 0){
    yl = ggplot2::expand_limits(y=0)
    xl <- ggplot2::scale_x_continuous(
      breaks = s$ybre, labels = s$scaleFUN, limits = c(0,s$tt1))
  } else {
      s1 <- easy.y.scales(my.graph.d1$y)
      yl = ggplot2::scale_y_log10(breaks=s1$ybre, limits=c(s1$ttlo,s1$tt1))
      if(max(my.graph.d1$x) > 1){
        ttlo <- 1
      } else {
        ttlo <- .001
      }
      xl <- ggplot2::scale_x_log10(
        breaks = s1$ybre, labels = s1$scaleFUN, limits = c(ttlo,s$tt1))
  }
  #
  # add a single line for small input, add line with ribbon otherwise
  # 
  if (ncol(my.graph.d1) == 3) {
   myline <- ggplot2::ggplot(
      my.graph.d1, ggplot2::aes(x = x, y = y, group = as.factor(sampleid))
    ) + 
      ggplot2::geom_line(ggplot2::aes(color=as.factor(sampleid)))
  } else {
    myline <- ggplot2::ggplot(
      my.graph.d1, ggplot2::aes(
        x = x, y = y, ymax = hi1, 
        ymin = lo1, group = as.factor(sampleid)
        )
    ) + 
      ggplot2::geom_line(
        ggplot2::aes(color=as.factor(sampleid))
        ) +
      ggplot2::geom_ribbon(
        alpha=0.25,ggplot2::aes(fill=as.factor(sampleid))
        ) +
      ggplot2::guides(fill=FALSE)
  }
  #
  if (m.legend == FALSE){
    ml <- ggplot2::theme(legend.position="none")
  } else{
    ml <- ggplot2::theme()
  }
  p <- myline +
    colo + ggplot2::labs(
      title = my.title, x = my.xlab,
      y = my.ylab, color=legend.title
      ) +
    theme1 + 
    ggplot2::theme(
      aspect.ratio = m.aspect.ratio
      ) + 
    ml + xl + yl#
  #
  if (drawme == TRUE){
    p
  }
  #
  return(p)
}