read_PAMAS <- function (file){
  # this function reads the PAMAS txt files. It returns a data frame with all
  # measurements events by time and by replicate. Provides information on user
  # profile and measured volume.
  
  pam <- readLines(file)
  profsline <- grep("ProfileName:\t", pam, value = FALSE) # used profiles
  profs <- grep("ProfileName:\t", pam, value = TRUE) # used profiles
  profs <- strsplit(profs, split = "\t", fixed = TRUE)
  profs <- as.data.frame(matrix(unlist(profs), ncol = 2, byrow = TRUE), 
                         stringsAsFactors = FALSE)
  msVol <- grep("MeasVolume:", pam, value = TRUE) # sampled volume per measurement
  msVol <- strsplit(msVol, split = "\t", fixed = TRUE)
  msVol <- as.data.frame(matrix(unlist(msVol), ncol = 2, byrow = TRUE), 
                         stringsAsFactors = FALSE)
  
  info <- data.frame(line=profsline, profile=profs[,2], volume=as.numeric(msVol[,2]))
  remove(msVol, profs)
  
  blocks <- grep("Block", pam, value = FALSE)  # line numbers were measurements start
  
  counts <-NULL
  
  for (i in 1:length(blocks)){
    prof_vol <- subset(info, line<blocks[i])
    prof_vol <- prof_vol[nrow(prof_vol),]
    
    blck <- pam[(blocks[i]+2):(blocks[i]+15)]
    blck <- strsplit(blck, split = "\t", fixed = TRUE)
    blck <- as.data.frame(matrix(unlist(blck), ncol = 41, byrow = TRUE), 
                          stringsAsFactors = FALSE)
    colnames(blck) <- blck[1,]
    blck <- blck[-1,]
    blck[,2] <- as.POSIXct(strptime(paste(blck[,1],' ', blck[,2]), format = '%m/%d/%Y %H:%M:%S'))
    blck[,1] <- i
    
    chamber_ID <- data.frame(chamber_ID = seq(1,nrow(blck),1))
    chamber_ID$profile <- prof_vol[1,2]
    chamber_ID$volume <- prof_vol[1,3]
    
    blck <- cbind(chamber_ID, blck)
    colnames(blck)[4]<-'block'
    colnames(blck)[5]<-'Date_Time'
    blck[,6:41]<-data.frame(lapply(blck[,6:41],as.numeric))
    if (is.null(counts)){
      counts <- blck
    }else{
      counts <- rbind(counts, blck)
    }
  }
  counts <- counts[,1:(ncol(counts)-7)]
  return(counts)
} 






