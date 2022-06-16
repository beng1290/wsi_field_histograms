##' import csv file
#' 
#' create by : benjamin green
#' 
#' import the cd8 count csv file
#' 
#' 
rn <- function(
  wd = 'E:/working_code/methods_workspace/wsi_field_histograms',
  fname = 'fields_vs_CD8Dens_wsi.csv', # 'prop_vs_CD8Counts.csv'; 'fields_vs_CD8Dens.csv'; 'prop_vs_CD8Dens.csv'
  type = 'Number', # 'proportion'
  z = '', #'zoomed' ,
  logl= 0
){
  #
  if (logl==1){logln = 'log'}else{logln=''}
  print('reading table')
  tbl <- data.table::fread(paste0(wd, '/', fname))
  tbl <- dplyr::arrange(tbl, sampleid, field_rank)
  tbl$ptype_count2 <-  ave(tbl$ptype_count, tbl$sampleid, FUN=cumsum)
  #
  # graph the cohort as line plots
  #
  print('computing counts plots')
  po <- all.line.plots(wd, tbl, type, type2 = 'Density', z, logl= logl)
  #
  # print
  #
  print('printing counts plots')
  gout <- gridExtra::marrangeGrob(
    grobs=po$p[1:4],
    nrow=2, ncol=2, top=NULL
  )
  #
  gout1 <- gridExtra::marrangeGrob(
    grobs=po$p[5:length(po$p)],
    nrow=3, ncol=3, top=NULL
  )
  #
  gout <- gridExtra::marrangeGrob(c(gout, gout1), 
                                  nrow = 1, ncol = 1, top =NULL)
  
  str = paste0(
    wd, '\\graphs\\graph_cellcounts_wholefields_',z,
    logln,'_', gsub('csv','pdf', fname))
  #
  ggplot2::ggsave(
    paste0(str,'.pdf'),gout, height = 9,
    width = 11, units = 'in', scale = 1, dpi = 300
  )
  #
  print('printing cumsum plots')
  #
  gout1 <- gridExtra::marrangeGrob(
    grobs=po$p2,
    nrow=3, ncol=3, top=NULL
  )
  #
  gout <- gridExtra::marrangeGrob(gout1, 
                                  nrow = 1, ncol = 1, top =NULL, logl= logl)
  
  str = paste0(
    wd, '\\graphs\\graph_cumsum_cellcounts_wholefields_',z,
    '_', gsub('csv','pdf', fname))
  #
  ggplot2::ggsave(
    paste0(str,'.pdf'),gout, height = 9,
    width = 11, units = 'in', scale = 1, dpi = 300
  )
  #
  print('computing prop plots')
  #
  a<-dplyr::mutate(
    dplyr::group_by(tbl, sampleid),
    ptype_count = ptype_count / sum(ptype_count)
  )
  a <- dplyr::arrange(a, sampleid, field_rank)
  a$ptype_count2 <-  ave(a$ptype_count, a$sampleid, FUN=cumsum)
  
  #
  # graph the rest of the cohort as line plots
  #
  po <- all.line.plots(wd, a, type, type2 = 'Proportions', z)
  #
  # print
  #
  print('printing prop plots')
  #
  gout <- gridExtra::marrangeGrob(
    grobs=po$p[1:4],
    nrow=2, ncol=2, top=NULL
  )
  #
  gout1 <- gridExtra::marrangeGrob(
    grobs=po$p[5:length(po$p)],
    nrow=3, ncol=3, top=NULL
  )
  #
  gout <- gridExtra::marrangeGrob(c(gout, gout1), 
                                  nrow = 1, ncol = 1, top =NULL)
  #
  str = paste0(
    wd, '\\graphs\\graph_cellproportions_wholefields_',z,
    logln,'_', gsub('csv','pdf', fname))
  #
  ggplot2::ggsave(
    paste0(str,'.pdf'),gout, height = 9,
    width = 11, units = 'in', scale = 1, dpi = 300
  )
  #
  print('printing cumsum prop plots')
  #
  gout1 <- gridExtra::marrangeGrob(
    grobs=po$p2,
    nrow=3, ncol=3, top=NULL
  )
  #
  gout <- gridExtra::marrangeGrob(gout1, 
                                  nrow = 1, ncol = 1, top =NULL)
  #
  str = paste0(
    wd, '\\graphs\\graph_cumsum_cellproportions_wholefields_',z,
    '_', gsub('csv','pdf', fname))
  #
  ggplot2::ggsave(
    paste0(str,'.pdf'),gout, height = 9,
    width = 11, units = 'in', scale = 1, dpi = 300
  )
}