bin.data<- function(tblin){
  #
  
    s = .01
  #
    tblout <- dplyr::mutate(
      tblin, sampleid, r = ceiling(field_rank / s)*s
    )
    tblout <- dplyr::summarise(
      dplyr::group_by(tblout, sampleid, r),
      ptype_count =sum(ptype_count),ptype_count2 =sum(ptype_count2),
      .groups = 'drop'
    )
  #
  colnames(tblout) <- c('sampleid','field_rank','ptype_count','ptype_count2')
  return(tblout)
}