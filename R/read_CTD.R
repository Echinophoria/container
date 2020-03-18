#'@export
read_CTD <- function(file){
  # reading raw file from CTD and create a searchable data.table
  # variables preserve original file names.
  ctd <- readLines(file, n=20)  # read the first few lines to get header info
  in.no <- ctd[1]
  in.no <- substr(in.no, start=(unlist(gregexpr(pattern=':;', in.no))+2), stop=nchar(in.no))
  hd<-grep('Ser;Meas', ctd, value = FALSE)  # the header size
  nms <- ctd[hd]
  hdr<-read.table(textConnection(nms), sep=';', header=TRUE)
  hdr<-names(hdr)
  ctd<-read.table(file, skip=hd, sep=';', header=FALSE)
  names(ctd)<-hdr[1:ncol(ctd)]
  ctd$Date <- strptime(ctd$Date, format = '%d.%m.%Y')
  ctd$Date <- as.POSIXct(ctd$Date)
  ctd$Time <- format(ctd$Time, format="%H:%M:%S")
  ctd=data.table(ctd) # to make it searchable

  ctd$instrument_id <- in.no
  # reorganise and rename

  if(ncol(ctd)>11){ # ctd with fluorometer and turbidity
    ctd <- select(ctd, instrument_id, Date, Time, Sal., Cond., Temp, F..µg.l., T..FTU., Density, S..vel., Press)
    names(ctd)[2:11]<-c('date', 'time', 'salinity', 'conductivity', 'temperature', 'fluorescense', 'turbidity', 'density', 'sound.velocity', 'pressure')
  }else{ # ctd w/o fluorometer + turbidity
    ctd <- select(ctd, Instrument_ID, Date, Time, Sal., Cond., Temp, Density, S..vel., Press)
    names(ctd)[2:9]<-c('date', 'time', 'salinity', 'conductivity', 'temperature', 'density', 'sound.velocity', 'pressure')
  }
  return(ctd)
}
