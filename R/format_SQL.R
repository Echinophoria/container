format_SQL <- function(ctd){
  ctd$datetime <- paste(as.character(ctd$datetime), "+0", sep='')  # ctd datetime is UTC

  if (ncol(ctd)<13){
    rows<- paste(ctd[,1],", ","'",ctd[,2],"'",", ",ctd[,3],", ",ctd[,4],", ",ctd[,5],", ",ctd[,6],", ",
                 ctd[,7],", ",ctd[,8],", ","'",ctd[,9],"'",", ",ctd[,10],", ",ctd[,11], sep='')

  }else{
    rows<- paste(ctd[,1],", ","'",ctd[,2],"'",", ",ctd[,3],", ",ctd[,4],", ",ctd[,5],", ",ctd[,6],", ",
                 ctd[,7],", ",ctd[,8],", ",ctd[,9],", ",ctd[,10],", ","'",ctd[,11],"'",", ",ctd[,12],", ",ctd[,13], sep='')
  }
  rows <- paste0('(', rows, ')')
  return(rows)
}

