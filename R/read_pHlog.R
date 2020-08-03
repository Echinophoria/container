#'@export

read_pHlog <- function(file){
  # reading raw file from PyroScience pH logger and create a searchable data.table
  # variables preserve original file names.
  require(data.table)
  pHlog <- readLines(file, n=25)  # read the first few lines to get header info
  log_name<-grep('#Log Nam', pHlog, value = TRUE)  # the header size
  log_name<-substr(log_name, (gregexpr('\t', log_name)[[1]]+1), nchar(log_name))

  st <- grep('#settings', pHlog, value = FALSE)
    lnm<-unlist(strsplit( pHlog[st], split='\t'))[-1]
    settings<-unlist(strsplit(pHlog[st+1],split='\t'))[-1]
    settings<-as.numeric(settings)
    names(settings)<-lnm
    settings<-list(settings)

  cal <- grep('#calibration', pHlog, value = FALSE)
    lnm<-unlist(strsplit(pHlog[cal], split='\t'))[-1]
    calibration<-unlist(strsplit(pHlog[cal+1],split='\t'))[-1]
    calibration<-as.numeric(calibration)
    names(calibration)<-lnm
    calibration<-list(calibration)

  hd <- grep('DateTime', pHlog, value = FALSE)
  nms <- grep('DateTime', pHlog, value = TRUE)

  hdr<-read.table(textConnection(nms), sep=c('\t'), header=TRUE)
  hdr<-names(hdr)
  vars<-c()
  units<-c()
  for (i in 1:length(hdr)){
    h <- hdr[i]
    h <- gsub(pattern = "\\.{2}", replacement = "\t",
         h)
    h <- strsplit(h, '\t')
    vars[i]<-h[[1]][1]
    units[i]<-h[[1]][2]
  }

  names(units)<- vars
  units<- list(units)

  log<-read.table(file, skip=hd, sep='\t', header=FALSE)
  names(log)<-vars

  log$DateTime <- strptime(log$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="")  #pH loggers use local timezone
  log$DateTime <- as.POSIXct(log$DateTime)
  attr(log$DateTime, "tzone") <- "UTC"  # change the time to UTC/GMT to be consistent with the other instruments.

  pHlog<-list(log=log, settings=settings, calibration=calibration, variable_units=units)
  return(pHlog)
}
