require(container)

data_locs <- data.frame(loc=c('Austevoll','Ulvik','Varadsoy', 'Leroy_Kaland'),
                        latitud = c(60.087924,60.560400, 60.129659, 60.471565),
                        longitud= c(5.262556, 6.9782625, 5.903513, 6.764756))

##### introduce CTD data ####

files<- list.files('G:/CTD_files', full.names = TRUE, pattern='.txt')


for (i in 2:length(files)){
  file <- files[i]
  loc_name <- c()
  for(j in 1:nrow(data_locs)){ # check which location is the file from
    a<-grep(data_locs[j,1], file, value=FALSE)
    if (length(a)>0){
      loc_name[j] <- a
    }else{
      loc_name[j] <- 0
    }

    }

  loc_name <- which(loc_name>0)
  loc <- as.character(data_locs[loc_name,1])
  coord<- c(data_locs[loc_name,2],data_locs[loc_name,3])
  CTD2SQL(file, loc, coord, pw='LTA')
}


##### introduce pH_logger data #####

files <- list.files('G:/CTD_files/pH', full.names = TRUE, pattern='.txt')
for (i in 2:length(files)){
  file <- files[i]
  loc_name <- c()
  for(j in 1:nrow(data_locs)){ # check which location is the file from
    a<-grep(substr(data_locs[j,1], 1, 5 ), file, value=FALSE, ignore.case = TRUE)
    if (length(a)>0){
      loc_name[j] <- a
    }else{
      loc_name[j] <- 0
    }

  }

  loc_name <- which(loc_name>0)
  loc <- as.character(data_locs[loc_name,1])
  coord<- c(data_locs[loc_name,2],data_locs[loc_name,3])
  CTD2SQL(file, loc, coord, pw='LTA')
}


##### TEMPLATE TO CREATE DATASETS IN POSGRESQL ####
# specifies the details of the table
sql_command <- "CREATE TABLE cartable
(
  location character varying NOT NULL,
  datetime timestamp[()],
  cyl numeric(1,0),
  disp numeric(4,1),
  hp numeric(3,0),
  drat numeric(3,2),
  wt numeric(4,3),
  qsec numeric(4,2),
  vs numeric(1,0),
  am numeric(1,0),
  gear numeric(1,0),
  carb numeric(1,0),
)
WITH (
  OIDS=FALSE
);
ALTER TABLE cartable
  OWNER TO LTA_team;
"
# sends the command and creates the table
dbGetQuery(con, sql_command)


#example to get data from database

df_postgres <- dbGetQuery(con, "SELECT date, time, salinity, temperature FROM ctd WHERE location ='Austevoll' AND date>'2020.03.01'")

df_postgres <- dbGetQuery(con, "SELECT * FROM ctd")


