#'@export
get_CR <- function(data, control, flow_rates, ranges=NULL){
  # this function estimates the clearance rate by particle size. it needs the
  # pamas readout from function read_PAMAS(), a value or vector with the
  # readings ID for the controls and a dataset containing the flowrates of each
  # chamber at date and time. It also takes a vector with the ranges, either
  # from size a to b, or larger or smaller than. If no ranges are given the
  # function estimates clearance rates for each interval and for the total (from
  # smaller size)

  # assign a flow rate to each count entry
  data$flow = 0
  for (i in 1:nrow(data)){
    chmb <- data[i,1]
    time <- data[i,5]
    t <- flow_rate[,2]
    y <- flow_rate[,(chmb+2)]
    app <- approx(t,y,xout=time)
    data[i,ncol(data)] <- app$y
  }

  # create and input data_set from the controls. Use linear interpolation between controls
  blocks <- unique(data$block)  # number of time replicates in out file
  control <- sort(control)
  input <- NULL
  for (i in 1:length(blocks)){
    out <- subset(data, block==blocks[i])
    ctrl <- out[1,]
    for (j in 1:length(control)){
      ctrl[j,] <-out[which(out$chamber_ID==control[j]),]  ## maybe add something to check outliers!
    }
    init <- out
    tout <- out[,5]
      for(k in 6:ncol(out)){
        t <- ctrl[,5]
        y <- ctrl[,k]
        interpol<- approx(t, y, xout = tout)
        init[,k] <- interpol[2]
      }
    if (is.null(input)){
      input <- init
    }else{
      input <- rbind(input, init)
    }
    }

  ## retained particles (N/ml)
  for (i in 1:nrow(data)){
    chamber<-input[i,1]
    vol <- input[i,3]
    init <- input[i,6:(ncol(input)-1)]
    out <- data[i,6:(ncol(data)-1)]
    iscontrol <- control[which(control==chamber)] # checking if the entry is a control
    if (length(iscontrol)>0){
      data[i,6:(ncol(input)-1)] <- init/vol
    }else{
      data[i,6:(ncol(input)-1)] <- (init-out)/vol
    }
  }

  ## applying ranges

  if (is.null(ranges)){  #perform clearance rate for each size class and total
    nm <- names(data)
    n1 <- nm[1]
    nm <- nm[6:(length(nm)-1)]
    nm2 <- c(nm,'')
    nm  <- c('',nm)
    nm2 <- sub('>', nm2, replacement='')
    nm <- paste(nm2, nm, sep='')
    nm[1] <- n1
    nm <- nm[1:(length(nm)-1)]

    retain <- data
    for (i in 7:(ncol(retain)-1)){
      retain[,i] <- data[,i-1]-data[,i]
    }
    colnames(retain)[6:(ncol(retain)-1)]=nm

    input_range <- input
    for (i in 7:(ncol(input_range)-1)){
      input_range[,i] <- (input[,i-1]-input[,i])/10
    }
    colnames(input_range)[6:(ncol(input_range)-1)]=nm

  }

  ## retention rate (N/min)
  retention_rate <- retain
  for (i in 1:nrow(retain)){
    chamber<-retain[i,1]
    flow <- retain[i,ncol(retain)]
    iscontrol <- control[which(control==chamber)] # checking if the entry is a control
    if (length(iscontrol)>0){
      retention_rate[i,6:(ncol(retain)-1)] <- retention_rate[i,6:(ncol(retain)-1)]
    }else{
      retention_rate[i,6:(ncol(retain)-1)] <- retention_rate[i,6:(ncol(retain)-1)]*flow
    }
  }

  ## clearance rate (ml/min)
  clearance_rate <- retention_rate
  clearance_rate[,6:(ncol(clearance_rate)-1)] <- retention_rate[,6:(ncol(retention_rate)-1)]/input_range[,6:(ncol(input_range)-1)]

  input_range <- subset(input_range, chamber_ID!=control[1] & chamber_ID!=control[2])
  retention_rate <- subset(retention_rate, chamber_ID!=control[1] & chamber_ID!=control[2])
  clearance_rate <- subset(clearance_rate, chamber_ID!=control[1] & chamber_ID!=control[2])


  results<-list(input_range, retention_rate, clearance_rate)
  names(results)<-c('In concentration, N/ml', 'Retention rate, N/min', 'Clearance rate, ml/min')
  return(results)
}
