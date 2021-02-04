#'@export
#'@import "RPostgreSQL"

upload_CTD <- function(file, loc, user, pw) {
  # load the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the database
  con <- dbConnect(drv, dbname="LTA_team",
                   host = 'postgres.hi.no', port=5432,
                   user = user, password=pw)

  commandSQL <- paste("SELECT latitude, longitude FROM locations WHERE location in ('", loc, "')",
                                   sep='')
  coords <- dbGetQuery(con, commandSQL)

  #ctd <- file #read_CTD(file)
  ctd$Ser <- NULL
  ctd$location <- loc

  ctd$latitude <- coords[1,1]
  ctd$longitude <- coords[1,2]

  #ctd <- subset(ctd, pressure>=1.5)

  rows <- format_SQL(ctd)

  # SQL statement
  statement <- paste0(
    "INSERT INTO ctd (",
    paste0(colnames(ctd), collapse = ', '),
    ')',
    ' VALUES ',
    paste0(rows, collapse = ', '),
    ' ON CONFLICT DO NOTHING'
  )

  info <- dbSendQuery(con, statement)

  dbDisconnect(con)
  return(info)
}



# finish preps and upload to database avoiding duplicates.
