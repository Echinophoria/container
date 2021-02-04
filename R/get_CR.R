#'@export
#'@importFrom "plyr" "summarise"
#'@importFrom "robustbase" "colMedians"
get_CR <- function(data, control, ctrl_type='mean'){
  # this function estimates the clearance rate by particle size. it needs the
  # pamas readout from function read_PAMAS(), a value or vector with the
  # readings ID for the controls and a dataset containing the flowrates of each
  # chamber at date and time. It also takes a vector with the ranges, either
  # from size a to b, or larger or smaller than. If no ranges are given the
  # function estimates clearance rates for each interval and for the total (from
  # smaller size)



  # create and input data_set from the controls. Use linear interpolation between controls

  data<- data[order(data$Date_Time),]
  rownames(data) <- NULL

  data$sorting <- paste(data$block, round(data$Date_Time, 'days'), sep='_')
  blocks <- unique(data$sorting)  # number of time replicates in out file

  control <- sort(control)
  input <- NULL



    # alternative use of the median between controls (more than 2 or mean when only 2) and apply to all the same value
  bad_blocks=c()
  for (i in 1:length(blocks)){
      out <- subset(data, sorting==blocks[i])
      ctrl <- out[which(out$chamber_ID==control[1] | out$chamber_ID==control[2]),]
      c2 <- subset(ctrl, flow>100)  # deleting controls that may had insuficient flow.
      c2 <- subset(c2, c2[,6]>1000)  # if the total counts are too low is because the pamas may been out of sync with arduino
      zero <- c2[which(c2[,6]<1000),6]
      if (nrow(c2)==0){ # there is no valid control and the whole block must be dropped.
        bad_blocks[i] <- blocks[i]
        median <- colMedians(as.matrix(ctrl[,7:(ncol(ctrl))-1]))[1:32]
      }else{
        median <- colMedians(as.matrix(c2[,7:(ncol(c2))-1]))[1:32]
      }
      if (length(zero)!=0){ # if one control doesn't measure anything then it is probably out of sync an the whole block must be deleted
        bad_blocks[i] <- blocks[i]
      }
      out[,6:37]<-matrix(rep(median, times=nrow(out)), ncol=length(median), byrow=TRUE)
      if (is.null(input)){
        input <- out
      }else{
        input <- rbind(input, out)
      }
  }
  bad_blocks <- bad_blocks[which(!is.na(bad_blocks))]
  input <- input[!input$sorting %in% bad_blocks, ] #deleting bad blocks without controls
  data <- data[!data$sorting %in% bad_blocks, ] #deleting bad blocks without controls
  input<- input[order(input$Date_Time),]
  rownames(input) <- NULL
  data$sorting=NULL
  input$sorting=NULL
  data<- data[order(data$Date_Time),]

  # delete rows where particles numbers are too small to be considered a reliable measurements <1000
  input <- input[which(data$X1.00>1000),]
  data <-data[which(data$X1.00>1000),]

  ## retained particles (N per ml)
  retain <- data
  retain[,6:(ncol(retain)-1)]<-(input[,6:(ncol(retain)-1)]-data[,6:(ncol(retain)-1)])/retain[,3]

  ## retention rate (N per minute)

  retention_rate <- retain
  retention_rate[,6:(ncol(retain)-1)]<-retain[6:(ncol(retain)-1)]*retain[,'flow'] # this one has to be kept

  ## retention efficiency (% retained)
  input[,6:(ncol(input)-1)]<-input[,6:(ncol(input)-1)]/input[,3]  # input as N/ml
  ret_eff <- retain
  ret_eff[,6:(ncol(retain)-1)] <- retain[6:(ncol(retain)-1)]/input[,6:(ncol(retain)-1)]  # this one too

  ## clearance rate (ml/min)

  clearance_rate <- retention_rate
  clearance_rate[,6:(ncol(clearance_rate)-4)] <- retention_rate[,6:(ncol(retention_rate)-4)]/input[,6:(ncol(input)-4)]


  retention_rate <- subset(retention_rate, chamber_ID!=control[1] & chamber_ID!=control[2])
  retention_eff  <- subset(ret_eff, chamber_ID!=control[1] & chamber_ID!=control[2])
  clearance_rate <- subset(clearance_rate, chamber_ID!=control[1] & chamber_ID!=control[2])


  results<-list(input, retention_eff, retention_rate, clearance_rate)
  names(results)<-c('In concentration, N/ml', 'Retention efficiency, -', 'Retention rate, N/min', 'Clearance rate, ml/min')
  return(results)
}
