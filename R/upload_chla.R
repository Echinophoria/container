#'@export
#'@import "RPostgreSQL"

upload_chla <- function(df, loc, user, pw) {
  # load the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the database
  con <- dbConnect(drv, dbname="LTA_team",
                   host = 'postgres.hi.no', port=5432,
                   user = user, password=pw)

  rows <- format_SQL(df)

  # SQL statement
  statement <- paste0(
    "INSERT INTO ctd (",
    paste0(colnames(ctd), collapse = ', '),
    ')',
    ' VALUES ',
    paste0(rows, collapse = ', '),
    ' ON CONFLICT DO UPDATE chla '
  )

  info <- dbSendQuery(con, statement)

  dbDisconnect(con)
  return(info)
}




# finish preps and upload to database avoiding duplicates.
