#'@export
#'@importFrom "plyr" "summarise"
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
    for (i in 1:length(blocks)){
      out <- subset(data, sorting==blocks[i])
      ctrl <- out[which(out$chamber_ID==control[1] | out$chamber_ID==control[2]),]
      average<-colMeans((ctrl[,7:(ncol(ctrl))-1]))[1:32]
      out[,6:37]<-matrix(rep(average, times=nrow(out)), ncol=length(average), byrow=TRUE)
      if (is.null(input)){
        input <- out
      }else{
        input <- rbind(input, out)
      }
    }
  # data<- data[order(data$Date_Time),]
  # rownames(input) <- NULL


  data$sorting=NULL
  input$sorting=NULL

  ## retained particles (N per ml)

  retain <- data
  retain[,6:(ncol(retain)-3)]<-(input[,6:(ncol(retain)-3)]-data[,6:(ncol(retain)-3)])/retain[,3]

  ## retention rate (N per minute)

  retention_rate <- retain
  retention_rate[,6:(ncol(retain)-3)]<-retain[6:(ncol(retain)-3)]*retain[,(ncol(retain)-2)] # this one has to be kept

  ## retention efficiency (% retained)
  input[,6:(ncol(input)-3)]<-input[,6:(ncol(input)-3)]/input[,3]  # input as N/ml
  ret_eff <- retain
  ret_eff[,6:(ncol(retain)-3)] <- retain[6:(ncol(retain)-3)]/input[,6:(ncol(retain)-3)]  # this one too


  ## clearance rate (ml/min)

  clearance_rate <- retention_rate
  clearance_rate[,6:(ncol(clearance_rate)-3)] <- retention_rate[,6:(ncol(retention_rate)-3)]/input[,6:(ncol(input)-3)]


  retention_rate <- subset(retention_rate, chamber_ID!=control[1] & chamber_ID!=control[2])
  retention_eff  <- subset(ret_eff, chamber_ID!=control[1] & chamber_ID!=control[2])
  clearance_rate <- subset(clearance_rate, chamber_ID!=control[1] & chamber_ID!=control[2])


  results<-list(input, retention_eff, retention_rate, clearance_rate)
  names(results)<-c('In concentration, N/ml', 'Retention efficiency, -', 'Retention rate, N/min', 'Clearance rate, ml/min')
  return(results)
}
