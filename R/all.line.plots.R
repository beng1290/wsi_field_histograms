all.line.plots <- function(wd, tbl, type, type2='Counts', z, logl =0){
  #
  my.xlab <- paste0(type, ' of Fields')
  if (type2 == 'Counts'){
    my.ylab <- '# of CD8 Cells'
  } else if (type2 == 'Proportions'){
    my.ylab <- 'Proportion of CD8 Cells'
  } else {
    my.ylab <- 'Dens of CD8 Cells'
  }
  #
  if (type == 'props'){
    tbl <-bin.data(tbl)
  }  #
  #
  # by cohort all tog-----------------------------------------------------------
  #
  tbl1 <- tbl[ which(tbl$sampleid == 999), ]
  my.title = paste0(type, ' of Fields vs CD8',type2,' Across the Cohort')
  my.graph.d1 <- dplyr::select(
    tbl1, c('sampleid','field_rank','ptype_count')
    )
  colnames(my.graph.d1) <- c('sampleid', 'x','y')
  my.graph.d1 <- dplyr::arrange(my.graph.d1, sampleid, x)
  #
  p = list(
    create.clean.line(
      my.graph.d1, m.aspect.ratio = .5,
      my.title = my.title,
      my.xlab = my.xlab, my.ylab=my.ylab, z = z, logl =logl
    )
  )
  #
  my.graph.d2 <- dplyr::select(
    tbl1, c('sampleid','field_rank','ptype_count2')
  )
  colnames(my.graph.d2) <- c('sampleid', 'x','y')
  #
  p2 = list(
    create.clean.line(
      my.graph.d2, m.aspect.ratio = .5,
      my.title = my.title,
      my.xlab = my.xlab, my.ylab=my.ylab, z = z
    )
  )
  #
  tbl2 <- data.table::fread(paste0(wd, '/clinical.csv'))
  tblb <- merge(tbl, tbl2, by = 'sampleid')
  #
  tblb <- dplyr::arrange(tblb, sampleid, field_rank)
  #
  # by cohort IQR & m-----------------------------------------------------------
  #
  seps <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  out <- subset.whole.cohort(tblb, seps, type)
  tbln <- tblb
  tbln$ptype_count <- tbln$ptype_count2
  out2 <- subset.whole.cohort(tbln, seps, type)
  #
  my.title = paste0(type, ' of Fields vs CD8',type2,' Across the Cohort')
  my.graph.d1 <- out$out1
  my.graph.d1 <- dplyr::arrange(my.graph.d1, sampleid, x)
  p = c(
    p, list(
      create.clean.line(
        my.graph.d1, m.aspect.ratio = .5,
        my.title = my.title,
        my.xlab = my.xlab, my.ylab=my.ylab, z = z, logl =logl
      )
    )
  )
  #
  my.graph.d2 <- my.graph.d1
  my.graph.d2 <- dplyr::arrange(my.graph.d2, sampleid, x)
  my.graph.d2$y <-  ave(my.graph.d2$y, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$hi1 <-  ave(my.graph.d2$hi1, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$hi2 <-  ave(my.graph.d2$hi2, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$lo1 <-  ave(my.graph.d2$lo1, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$lo2 <-  ave(my.graph.d2$lo2, my.graph.d2$sampleid, FUN=cumsum)
  #
  if (type == 'Proportions'){
    my.graph.d2 <- dplyr::mutate(
      dplyr::group_by(
        my.graph.d2, sampleid
      ), y = y/max(y),
      hi1 = hi1/max(hi1),
      hi2 = hi2/max(hi2), 
      lo1 = lo1/max(lo1), 
      lo2 = lo2/max(lo2)
    )
  }
  #
  p2 = c(
    p2, list(
      create.clean.line(
        my.graph.d2, m.aspect.ratio = .5,
        my.title = my.title,
        my.xlab = my.xlab, my.ylab=my.ylab, z = z
      )
    )
  )
  # by response & coh-----------------------------------------------------------
  #
  my.title = paste0(type, ' of Fields vs CD8',type2,' Across the Cohort by Response')
  my.graph.d1 <- out$out2
  my.graph.d1 <- dplyr::arrange(my.graph.d1, sampleid, x)
  #
  p = c(
    p, list(
      create.clean.line(
        my.graph.d1, m.aspect.ratio = .5,
        my.title = my.title,
        my.xlab = my.xlab, my.ylab=my.ylab, z = z,
        m.legend = TRUE, legend.title = 'Response Status', logl =logl
      )
    )
  )
  #
  my.graph.d2 <- my.graph.d1
  my.graph.d2 <- dplyr::arrange(my.graph.d2, sampleid, x)
  my.graph.d2$y <-  ave(my.graph.d2$y, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$hi1 <-  ave(my.graph.d2$hi1, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$hi2 <-  ave(my.graph.d2$hi2, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$lo1 <-  ave(my.graph.d2$lo1, my.graph.d2$sampleid, FUN=cumsum)
  my.graph.d2$lo2 <-  ave(my.graph.d2$lo2, my.graph.d2$sampleid, FUN=cumsum)
  #
  if (type == 'Proportions'){
    my.graph.d2 <- dplyr::mutate(
      dplyr::group_by(
        my.graph.d2, sampleid
      ), y = y/max(y),
      hi1 = hi1/max(hi1),
      hi2 = hi2/max(hi2), 
      lo1 = lo1/max(lo1), 
      lo2 = lo2/max(lo2)
    )
  }
  #
  p2 = c(
    p2, list(
      create.clean.line(
        my.graph.d2, m.aspect.ratio = .5,
        my.title = my.title,
        my.xlab = my.xlab, my.ylab=my.ylab, z = z,
        m.legend = TRUE, legend.title = 'Response Status'
      )
    )
  )
  #
  # each slide separa-----------------------------------------------------------
  my.title = paste0(type, ' of Fields vs CD8',type2,' All Samples')
  my.graph.d1 <- dplyr::select(
    tblb, c('sampleid','field_rank','ptype_count')
    )
  colnames(my.graph.d1) <- c('sampleid', 'x','y')
  my.graph.d1 <- dplyr::arrange(my.graph.d1, sampleid, x)
  #
  p = c(p, list(create.clean.line(my.graph.d1, m.aspect.ratio = .5,
                             my.title = my.title,
                             my.xlab = my.xlab, my.ylab=my.ylab, z = z, logl =logl))
  )
  #
  # tumor area cuts  -----------------------------------------------------------
  #
  for (i1 in 1:length(seps)){
    sepn <- seps[i1]
    my.title = paste0(type, ' of Fields vs CD8',type2,' by tArea ', sepn,'mm2')
    my.graph.d1 <- out$out3[[i1]]
    my.graph.d1 <- dplyr::arrange(my.graph.d1, sampleid, x)
    #
    p = c(
      p, list(
        create.clean.line(
          my.graph.d1, m.aspect.ratio = .5,
          my.title = my.title,
          my.xlab = my.xlab, my.ylab = my.ylab, z = z,
          m.legend = TRUE, legend.title = 'Response Status', logl =logl
        )
      )
    )
    #
    my.graph.d2 <- my.graph.d1
    my.graph.d2 <- dplyr::arrange(my.graph.d2, sampleid, x)
    my.graph.d2$y <-  ave(my.graph.d2$y, my.graph.d2$sampleid, FUN=cumsum)
    my.graph.d2$hi1 <-  ave(my.graph.d2$hi1, my.graph.d2$sampleid, FUN=cumsum)
    my.graph.d2$hi2 <-  ave(my.graph.d2$hi2, my.graph.d2$sampleid, FUN=cumsum)
    my.graph.d2$lo1 <-  ave(my.graph.d2$lo1, my.graph.d2$sampleid, FUN=cumsum)
    my.graph.d2$lo2 <-  ave(my.graph.d2$lo2, my.graph.d2$sampleid, FUN=cumsum)
    #
    if (type == 'Proportions'){
      my.graph.d2 <- dplyr::mutate(
        dplyr::group_by(
          my.graph.d2, sampleid
        ), y = y/max(y),
        hi1 = hi1/max(hi1),
        hi2 = hi2/max(hi2), 
        lo1 = lo1/max(lo1), 
        lo2 = lo2/max(lo2)
      )
    }
    #
    p2 = c(
      p2, list(
        create.clean.line(
          my.graph.d2, m.aspect.ratio = .5,
          my.title = my.title,
          my.xlab = my.xlab, my.ylab=my.ylab, z = z,
          m.legend = TRUE, legend.title = 'Response Status'
        )
      )
    )
  }
    #
  #
  # separate by cas  -----------------------------------------------------------
  #
  tblb <- dplyr::arrange(tblb, Response, sampleid, field_rank)
  tblb$Response[which(tblb$Response == 'Responder')] <- 'R'
  tblb$Response[which(tblb$Response == 'Non-Responder')] <- 'NR'
  #
  tbln <- dplyr::arrange(tbln, Response, sampleid, field_rank)
  tbln$Response[which(tbln$Response == 'Responder')] <- 'R'
  tbln$Response[which(tbln$Response == 'Non-Responder')] <- 'NR'
  #
  samples <- unique(tblb$sampleid)
  for (i1 in 1:length(samples)){
    #
    my.graph.d1 <- tblb[ which(tblb$sampleid == samples[i1]), ]
    r = my.graph.d1$Response[[1]]
    my.graph.d1 <- dplyr::select(
      my.graph.d1, c('sampleid','field_rank','ptype_count')
    )
    colnames(my.graph.d1) <- c('sampleid', 'x','y')
    my.graph.d1 <- dplyr::arrange(my.graph.d1, sampleid, x)
    my.title = paste0(
      type, ' of Fields vs CD8',type2,' Sampleid ', samples[i1],' ', r
      )
    p<- c(p, list(create.clean.line(
      my.graph.d1, m.aspect.ratio = .5, my.title = my.title,
      my.xlab = my.xlab, my.ylab=my.ylab, z = z, logl =logl)
    ))
    #
    #
    my.graph.d2 <- my.graph.d1
    my.graph.d2 <- dplyr::arrange(my.graph.d2, sampleid, x)
    my.graph.d2$y <-  ave(my.graph.d2$y, my.graph.d2$sampleid, FUN=cumsum)
    #
    if (type == 'Proportions'){
      my.graph.d2 <- dplyr::mutate(
        dplyr::group_by(
          my.graph.d2, sampleid
        ), y = y/max(y)
      )
    }
    #
    #
    p2 = c(
      p2, list(
        create.clean.line(
          my.graph.d2, m.aspect.ratio = .5,
          my.title = my.title,
          my.xlab = my.xlab, my.ylab=my.ylab, z = z
        )
      )
    )
  }
  return(list(p= p, p2= p2))
}