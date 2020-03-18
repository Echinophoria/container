#'@export
#'@import "RpostgreSQL"

CTD2SQL <- function(file, loc, coord, pw){
  # This function store CTD output files in postgreSQL database. It eases the
  # work with several or very large files allowing us to use queries on the data
  # we need at every moment


  # check that either location name or coordinates are given. Data without
  # location information is useless.
  if (!exists('loc') & !exists('coord')){
   stop('at least a location name or  coordinates need to be provided with the CTD data')
  }


  # read the CTD file into a df with the require entries and names
  ctd <- read_CTD(file)

  # adding location information
  if (exists('loc')){
    ctd$location <- loc
  }

  if(exists('coord')){
    ctd$latitude <- coord[1]
    ctd$longitude <- coord[2]
  }

  # connect to the database

  # load the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the database
  con <- dbConnect(drv, dbname="postgres",
                 host = 'localhost', port=5432,
                 user = 'LTA_team', password=pw)
  rm(pw)

  # check that CTD table is there
  if(!dbExistsTable(con, 'ctd')){
    stop('table does not exists withing database!!')
  }

  # pushing data to database
  ctd <- data.frame(ctd)
  dbWriteTable(con, 'ctd', value=ctd, append=TRUE, row.names=FALSE)

  on.exit(dbDisconnect(con))
}



