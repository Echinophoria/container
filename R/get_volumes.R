#'@export
get_volumes <- function(data){
  # this function reads any output from get_CR file and calculate the vol of each fraction
  # it assumes all particles are perfect spheres volume = 4/3 pi R^3

  nm <- names(data)[6:(ncol(data)-1)]
  ranges <- strsplit(nm, split = ">", fixed = TRUE)
  vol <- c()

  for (i in 1:length(ranges)){
    rg <- ranges[[i]]
    lw <- as.numeric(rg[1])
    up <- as.numeric(rg[2])
    if (is.na(lw)|is.na(up)){
     vol[i]<- NA
    }else{
      ratio <- (up + lw)/4
      vol[i] <- (4/3) * pi * ratio^3  # um^3, volumen per particle
    }
  }
  vol <- matrix(vol, nrow=nrow(data), ncol=length(vol), byrow=TRUE)
  data[,6:(ncol(data)-1)] <- data[,6:(ncol(data)-1)] * vol
  return(data)
}
