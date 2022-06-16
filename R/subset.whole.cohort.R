subset.whole.cohort <- function(tblb, seps, type){
  # -----------------------------------------------------------------
  # whole cohort
  out1 <- dplyr::summarise(
    dplyr::group_by(
      tblb, field_rank
    ),
    sampleid = 1,
    av = median(ptype_count),
    hi1 = quantile(ptype_count, .75),
    lo1 = quantile(ptype_count, .25),
    hi2 = max(ptype_count),
    lo2 = min(ptype_count),
    .groups = 'drop'
  )
  colnames(out1) <- c('x','sampleid','y','hi1','lo1','hi2','lo2')
  # -----------------------------------------------------------------
  # by response
  out2 <- dplyr::summarise(
    dplyr::group_by(
      tblb, field_rank, Response
    ),
    av = median(ptype_count),
    hi1 = quantile(ptype_count, .75),
    lo1 = quantile(ptype_count, .25),
    hi2 = max(ptype_count),
    lo2 = min(ptype_count),
    c = dplyr::n(),
    .groups = 'drop'
  )
  colnames(out2) <- c('x','sampleid','y','hi1','lo1','hi2','lo2','c')
  # -----------------------------------------------------------------
  # different tumor increment separators in 5 mm2 increments
  #seps <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  out3 <- list()
  #
  for (i1 in 1:length(seps)){
      sepn <- seps[i1]
      if (type == 'props'){
        tb1 <- tblb[(tblb$tumor_area >= sepn), ]
        tb2 <- tblb[(tblb$tumor_area < sepn), ]
      } else {
        tb1 <- tblb[(tblb$tumor_area.y >= sepn), ]
        tb2 <- tblb[(tblb$tumor_area.y < sepn), ]
      }
      tb1$mnew <- paste0(tb1$Response, '_Large')
      tb2$mnew <- paste0(tb2$Response, '_Small')
      tbn <- dplyr::summarise(
        dplyr::group_by(
          rbind(tb1, tb2),
          field_rank, mnew
        ),
        av = median(ptype_count),
        hi1 = quantile(ptype_count, .75),
        lo1 = quantile(ptype_count, .25),
        hi2 = max(ptype_count),
        lo2 = min(ptype_count),
        c = dplyr::n(),
        .groups = 'drop'
      )
      colnames(tbn) <- c('x','sampleid','y','hi1','lo1','hi2','lo2', 'c')
      out3 <- c(out3, list(tbn))
  }
  return(list(out1=out1, out2=out2, out3=out3))
}