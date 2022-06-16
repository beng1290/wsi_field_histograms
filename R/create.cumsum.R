#' create.comp.ksplots
#' created by: benjamin green 09/03/2020
#' 
#' plot out the cum dists and the KS test value
#' 
#' @param my.graph.d the data structure to be graphed
#' @param my.cols the columns to graph from my.graph.d
#' @return a list of dot plots
#' @export
#' 
create.cumsum <- function(my.graph.d2, drawme=TRUE, my.ylab='', my.xlab='',
                                  my.title='', m.aspect.ratio=1, m.legend = FALSE,
                                  legend.title='', m.labels = ''){
  #
  ## graph theme
  #
  theme1 <- my.theme()
  cols <- theme1$colors
  theme1 <- theme1$theme1
  #
  ybre <- seq(0, 1,.25)
  scaleFUN <- function(x) sprintf("%.2f", x)
  #
  j.width <- .3
  my.guides <- ggplot2::guides(fill = F)
  #
  ylb <- "Cumulative Probabality"
  xlb <- my.ylab
  tit <- my.title
  #
  # colors and legends
  #
  n.cols <- length(unique(my.graph.d2$sampleid))
  if (n.cols == 1){
    my.guides <- ggplot2::guides(fill = F, color =F)
    m.breaks = c('x')  
    m.labels = ''
    my.col.vals <- cols[1]
    plots<- 
      ggplot2::ggplot(
        my.graph.d2, ggplot2::aes(
          x = y, group = sampleid, colour = sampleid
        )
      )+
      ggplot2::stat_ecdf(size=.5) + 
      ggplot2::labs(
        x = xlb,
        y = ylb,
        title= tit) +
      ggplot2::scale_color_manual(
        values = my.col.vals, labels = m.labels, breaks = m.breaks
      ) +
      ggplot2::scale_fill_manual(
        values = my.col.vals, labels = m.labels, breaks = m.breaks
      ) +
      ggplot2::scale_y_continuous(breaks = ybre, labels = scaleFUN, limits = c(0,1)) +
      my.guides + ml+ ggplot2::expand_limits(x=0)+
      theme1 + ggplot2::theme( axis.title.x = ggplot2::element_text(size = 6),
                               axis.text.x=ggplot2::element_text(),
                               axis.ticks.x=ggplot2::element_line(), 
                               legend.position = c(1.24,.8),
                               legend.title = ggplot2::element_blank()) 
    
    
  } else {
    m.breaks = c('x','y')
    my.col.vals <- c(cols[2], cols[1])
    names(my.col.vals) <- m.breaks
    #
    x1 <- my.graph.d2$y[my.graph.d2$sampleid == 'x']
    y1 <- my.graph.d2$y[my.graph.d2$sampleid == 'y']
    #
    cdf1 <- ecdf(x1) 
    cdf2 <- ecdf(y1) 
    # find min and max statistics to draw line between points of greatest distance
    minMax <- seq(min(x1, y1), max(x1, y1), length.out=length(x1)) 
    x0 <- minMax[which(
      abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) 
    )] 
    ym <- cdf1(x0[[1]]) 
    ym2 <- cdf2(x0[[1]]) 
    #
    test.out <- suppressWarnings(
      ks.test(
        x1, y1, alternative = c('two.sided'), exact = FALSE
      )
    )
    #
    xn = (.55 * max(my.graph.d2$y))
    #
    my.graph.d2 <- dplyr::mutate(
      my.graph.d2,x0 = x0[[1]], ym = ym,ym2 = ym2, xn0 = xn
    )
    #
    plots<- 
      ggplot2::ggplot(
        my.graph.d2, ggplot2::aes(
          x = y, group = sampleid, colour = sampleid
        )
      )+
      ggplot2::stat_ecdf(size=.5) +
      ggplot2::geom_segment(
        ggplot2::aes(x = x0, y = ym, xend = x0, yend = ym2),
        linetype = "dashed", color = "grey", size = .5) +
      ggplot2::geom_point(
        ggplot2::aes(x = x0, y= ym), color="grey", size=.7) +
      ggplot2::geom_point(
        ggplot2::aes(x = x0, y= ym2), color="grey", size=.7) +
      ggplot2::annotate(
        "text", x = xn, y = .01,
        label = paste0('p =  ',  round(test.out$p.value, 3)), size = 3
      ) +
      ggplot2::annotate(
        "text", x = xn, y = .05,
        label = paste0('D =  ',  round(test.out$statistic, 3)), size = 3
      ) + 
      ggplot2::labs(
        x = xlb,
        y = ylb,
        title= tit) +
      ggplot2::scale_color_manual(
        values = my.col.vals, labels = m.labels, breaks = m.breaks
      ) +
      ggplot2::scale_fill_manual(
        values = my.col.vals, labels = m.labels, breaks = m.breaks
      ) +
      ggplot2::scale_y_continuous(breaks = ybre, labels = scaleFUN, limits = c(0,1)) +
      my.guides + ml+ggplot2::expand_limits(x=0)+
      theme1 + ggplot2::theme( axis.title.x = ggplot2::element_text(size = 6),
                               axis.text.x=ggplot2::element_text(),
                               axis.ticks.x=ggplot2::element_line(), 
                               legend.position = c(1.24,.8),
                               legend.title = ggplot2::element_blank())
  }
  if (drawme == TRUE){
    plots
  }
  #
  return(plots)
}