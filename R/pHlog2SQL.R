#'@export
#'@import "RpostgreSQL"

pHlog2SQL <- function(file, loc, coord, pw){
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

  # connect to the database

  # load the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the database
  con <- dbConnect(drv, dbname="postgres",
                   host = 'localhost', port=5432,
                   user = 'LTA_team', password=pw)
  rm(pw)

  # check that CTD table is there
  if(!dbExistsTable(con, 'ph_logger')){
    stop('table does not exists withing database!!')
  }

  # pushing data to database
#  ctd$index <- paste(ctd$location,ctd$date,ctd$time, sep='')
  dbWriteTable(con, 'ph_logger', value=pH, append=TRUE, row.names=FALSE)

}
