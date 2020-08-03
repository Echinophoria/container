#'@export

read_CTD <- function(file){
  # reading raw file from CTD and create a searchable data.table
  # variables preserve original file names.
  ctd <- readLines(file, n=500)  # read the first few lines to get header info
  in.no <- ctd[1]
  in.no <- substr(in.no, start=(unlist(gregexpr(pattern=':;', in.no))+2), stop=nchar(in.no))
  hd<-grep('Ser;Meas', ctd, value = FALSE)  # the header size
  nms <- ctd[hd]
  hdr<-read.table(textConnection(nms), sep=';', header=TRUE)
  hdr<-names(hdr)
  ctd<-read.table(file, skip=hd, sep=';', header=FALSE)
  names(ctd)<-hdr[1:ncol(ctd)]
  ctd$Date <- strptime(paste(ctd$Date, ctd$Time), format = '%d.%m.%Y %H:%M:%S', tz = "UTC")
  ctd$Date <- as.POSIXct(ctd$Date)
  ctd=data.table(ctd) # to make it searchable

  ctd$instrument_id <- in.no
  # reorganise and rename

  if(ncol(ctd)>11){ # ctd with fluorometer and turbidity
    ctd <- select(ctd, Ser, instrument_id, Date, Sal., Cond., Temp, F..µg.l., T..FTU., Density, S..vel., Press)
    names(ctd)[1:11]<-c('Ser','instrument_id','datetime', 'salinity', 'conductivity', 'temperature', 'fluorescence', 'turbidity', 'density', 'sound_velocity', 'pressure')
  }else{ # ctd w/o fluorometer + turbidity
    ctd <- select(ctd, Ser, instrument_id, Date, Sal., Cond., Temp, Density, S..vel., Press)
    names(ctd)[1:09]<-c('Ser', 'instrument_id','datetime', 'salinity', 'conductivity', 'temperature', 'density', 'sound_velocity', 'pressure')
  }
  return(ctd)
}
