#'@export
#'@import "RPostgreSQL"

CTDLF2SQL <- function(file, fluo_threshold, turb_threshold,  pw){
  # This function store CTD output files from Lysefjord transects in postgreSQL database. It eases the
  # work with several or very large files allowing us to use queries on the data
  # we need at every moment


  # check that either location name or coordinates are given. Data without
  # location information is useless.

  # read the CTD file into a df with the require entries and names
  ctd <- read_CTD(file)

  # delete surface data records
  ctd <- subset(ctd, pressure>1.2)



  # apply Fluorescence threshold
  if(exists('fluo_threshold')){
    ctd <- subset(ctd, fluorescence <= fluo_threshold)
  }

  if(exists('turb_threshold')){
    ctd <- subset(ctd, fluorescence <= turb_threshold)
  }



  series<- ddply(.data = ctd, c("Ser"),summarise,
                 N    = length(pressure),
                 Depth = max(pressure),
                 date = mean(datetime))

  # each date there must be 8 series, one for each location.
  series$r_date <- as.numeric(trunc(series$date, unit='day'))

  s2 <- ddply(.data = series, c("r_date"), summarise,
              N    = length(Depth),
              date = mean(date))

  if(mean(s2$N < 8)){
    stop(paste('Error one of the dates does not have 8 stations: ', s2$date, s2$N))
  }

  # adding location information
  locs <- read.table('lysefjord_stations.txt', header=TRUE, sep='\t')

  # each date there must be 8 series, one for each location.
  series$location = rep(locs$location, time= (nrow(series)/8))
  series$latitude = rep(locs$latitude, time= (nrow(series)/8))
  series$longitude = rep(locs$longitude, time= (nrow(series)/8))


  ctd$location <- series$location[match(ctd$Ser, series$Ser)]
  ctd$latitude <- series$latitude[match(ctd$Ser, series$Ser)]
  ctd$longitude <- series$longitude[match(ctd$Ser, series$Ser)]

  ctd$Ser <- NULL
  ctd$r_date <- NULL


  return(ctd)
}



