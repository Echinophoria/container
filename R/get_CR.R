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
  blocks <- unique(data$block)  # number of time replicates in out file
  control <- sort(control)
  input <- NULL
  if (ctrl_type!='mean'){
    for (i in 1:length(blocks)){
      out <- subset(data, block==blocks[i])
      ctrl <- out[control,]

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
  }else{

    # alternative use of the controls mean between controls and apply to all the same value
    for (i in 1:length(blocks)){
      out <- subset(data, block==blocks[i])
      ctrl <- out[control,]
      average<-colMeans(ctrl[,6:ncol(ctrl)])
      out[,6:38]<-matrix(rep(average, times=nrow(out)), ncol=length(average), byrow=TRUE)
      if (is.null(input)){
        input <- out
      }else{
        input <- rbind(input, out)
      }
    }
  }




  ## retained particles (N per ml)

  retain <- data
  retain[,6:(ncol(retain)-3)]<-(input[,6:(ncol(retain)-3)]-data[,6:(ncol(retain)-3)])/retain[,3]

  ## retention rate (N per minute)

  retention_rate <- retain
  retention_rate[,6:(ncol(retain)-3)]<-retain[6:(ncol(retain)-3)]*retain[,(ncol(retain)-2)]

  ## retention efficiency (% retained)
  input[,6:(ncol(input)-3)]<-input[,6:(ncol(input)-3)]/input[,3]
  ret_eff <- retain
  ret_eff[,6:(ncol(retain)-3)] <- retain[6:(ncol(retain)-3)]/input[,6:(ncol(retain)-3)]


  ## clearance rate (ml/min)

  clearance_rate <- retention_rate
  clearance_rate[,6:(ncol(clearance_rate)-3)] <- retention_rate[,6:(ncol(retention_rate)-3)]/input[,6:(ncol(input)-3)]


  retention_rate <- subset(retention_rate, chamber_ID!=control[1] & chamber_ID!=control[2])
  retention_eff  <- subset(ret_eff, chamber_ID!=control[1] & chamber_ID!=control[2])
  clearance_rate <- subset(clearance_rate, chamber_ID!=control[1] & chamber_ID!=control[2])


  results<-list(input, retention_eff, retention_rate, clearance_rate)
  names(results)<-c('In concentration, N/ml', 'Retentio efficiency, -', 'Retention rate, N/min', 'Clearance rate, ml/min')
  return(results)
}
