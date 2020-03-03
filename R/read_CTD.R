read_CTD <- function(file){
  # reading raw file from CTD and create a searchable data.table
  # variables preserve original file names.
  require(data.table)
  ctd <- readLines(file, n=20)  # read the first few lines to get header info
  hd<-grep('Ser;Meas', ctd, value = FALSE)  # the header size
  nms <- ctd[hd]
  hdr<-read.table(textConnection(nms), sep=';', header=TRUE)
  hdr<-names(hdr)
  ctd<-read.table(file, skip=hd, sep=';', header=FALSE)
  names(ctd)<-hdr[1:ncol(ctd)]
  ctd$Date_Time <- strptime(paste(ctd$Date,' ', ctd$Time), format = '%d.%m.%Y %H:%M:%S')
  ctd$Date_Time <- as.POSIXct(ctd$Date_Time)
  ctd=data.table(ctd) # to make it searchable
  return(ctd)
}