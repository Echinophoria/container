#'@export

read_PAMAS <- function (file){
  # this function reads the PAMAS txt files. It returns a data frame with all
  # measurements events by time and by replicate. Provides information on user
  # profile and measured volume. Calculates particles in each range. i.e. substracts the cumulative

  pam <- readLines(file)
  profsline <- grep("ProfileName:\t", pam, value = FALSE) # used profiles

  profs <- grep("ProfileName:\t", pam, value = TRUE) # used profiles
  profs <- strsplit(profs, split = "\t", fixed = TRUE)
  profs <- as.data.frame(matrix(unlist(profs), ncol = 2, byrow = TRUE),
                         stringsAsFactors = FALSE)
  sampleIDs <- grep("SampleID:\t", pam, value = TRUE) # used profiles
  sampleIDs <- strsplit(sampleIDs, split = "\t", fixed = TRUE)
  for (i in 1:length(sampleIDs)){
    a <- sampleIDs[[i]]
    if (length(a)<2){
      sampleIDs[[i]][2] = NA
    }
  }
  sampleIDs <-  as.data.frame(matrix(unlist(sampleIDs), ncol = 2, byrow = TRUE),
                              stringsAsFactors = FALSE)

  msVol <- grep("MeasVolume:", pam, value = TRUE) # sampled volume per measurement
  msVol <- strsplit(msVol, split = "\t", fixed = TRUE)
  msVol <- as.data.frame(matrix(unlist(msVol), ncol = 2, byrow = TRUE),
                         stringsAsFactors = FALSE)

  info <- data.frame(line=profsline, profile=profs[,2], volume=as.numeric(msVol[,2]), sampleID=sampleIDs[,2])
  remove(msVol, profs, sampleIDs)

  blocks <- grep("Block", pam, value = FALSE)  # line numbers were measurements start

  counts <-NULL

  for (i in 1:length(blocks)){
    prof_vol <- subset(info, line<blocks[i])
    prof_vol <- prof_vol[nrow(prof_vol),]


    nmeasures <- as.numeric(substr(pam[blocks[i]], nchar(pam[blocks[i]])-1, nchar(pam[blocks[i]]))) # number of measurements per block
    blck <- pam[(blocks[i]+2):(blocks[i]+nmeasures+2)]
    blck <- strsplit(blck, split = "\t", fixed = TRUE)
    if (length(blck[[1]])==38){ # the old pamas skip labels
      blck <- pam[(blocks[i]+2):(blocks[i]+nmeasures+3)]
      blck <- strsplit(blck, split = "\t", fixed = TRUE)
      blck[[1]] <- c(blck[[1]][1:(which(blck[[1]]=='->')-2)],">29.00", ">30.00",blck[[2]][2:3],blck[[1]][which(blck[[1]]=='->'):38])
      blck[[2]] <- NULL
    }
    blck <- as.data.frame(matrix(unlist(blck), ncol = length(blck[[1]]), byrow = TRUE),
                          stringsAsFactors = FALSE)
    colnames(blck) <- blck[1,]
    blck <- blck[-1,]
    blck[,2] <- as.POSIXct(strptime(paste(blck[,1],' ', blck[,2]), format = '%m/%d/%Y %H:%M:%S'))
    blck[,1] <- i

    chamber_ID <- data.frame(chamber_ID = seq(1,nrow(blck),1))
    chamber_ID$profile <- prof_vol[1,2]
    chamber_ID$volume <- prof_vol[1,3]
    chamber_ID$sampleID <- prof_vol[1,4]

    blck <- cbind(chamber_ID, blck)
    colnames(blck)[5]<-'block'
    colnames(blck)[6]<-'Date_Time'
    blck[,7:ncol(blck)]<-data.frame(lapply(blck[,7:ncol(blck)],as.numeric))
    if (is.null(counts)){
      counts <- blck
    }else{
      counts <- rbind.fill(counts, blck)
    }
  }
  counts <- counts[,1:(ncol(counts)-7)]

  #calculating numbers per range.
  nm <- names(counts)
  n1 <- nm[1]
  nm <- nm[7:(length(nm))]
  nm2 <- c(nm,'')
  nm  <- c('',nm)
  nm2 <- sub('>', nm2, replacement='')
  nm <- paste(nm2, nm, sep='')
  nm <- nm[1:(length(nm))]

  cts <- counts
  for (i in 8:(ncol(cts))){
    counts[,i] <- cts[,i-1]-cts[,i]
  }
  colnames(counts)[7:(ncol(counts))]=nm

  return(counts)
}






