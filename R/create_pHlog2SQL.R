#'@export

create_pHlog2SQL <- function(file, loc, coord){
  # This function store pH logger output files in postgreSQL database. It eases the
  # work with several or very large files allowing us to use queries on the data
  # we need at every moment


  # check that either location name or coordinates are given. Data without
  # location information is useless.
  if (!exists('loc') & !exists('coord')){
    stop('at least a location name or  coordinates need to be provided with the CTD data')
  }


  # read the CTD file into a df with the require entries and names
  pH <- read_pHlog(file)
  pH <- data.frame(pH$log)
  pH <- select(pH, DateTime, ambientLight, tempSample, pH)


  # adding location information
  if (exists('loc')){
    pH$location <- loc
  }

  if(exists('coord')){
    pH$latitude <- unlist(coord[1])
    pH$longitude <- unlist(coord[2])
  }

  names(pH)[1:4]<-c('datetime', 'light', 'temperature','ph')
  pH$temperature <- pH$temperature/1000
  pH$ph <- pH$ph/1000

  # clean outliers
  pH <- subset(pH, ph>0 & ph<10)
  pH[which(pH$light>9999),'light'] <- 9999

return(pH)

}
