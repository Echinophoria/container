require(container)
data_locs <- data.frame(loc=c('Austevoll','Ulvik','Varadsoy', 'Leroy_Kaland'),
                        latitud = c(60.087924,60.560400, 60.129659, 60.471565),
                        longitud= c(5.262556, 6.9782625, 5.903513, 6.764756))

##### introduce CTD data ####

files<- list.files('G:/CTD_files', full.names = TRUE, pattern='.txt')


for (i in 6:length(files)){
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
for (i in 4:length(files)){
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
  pHlog2SQL(file, loc, coord, pw='LTA')
}




##### SESTON properties data ####








##### TEMPLATE TO CREATE DATASETS IN POSGRESQL ####
# specifies the details of the table
sql_command <- "CREATE TABLE carbon_chemistry
(
  location character varying NOT NULL,
  latitude numeric(9,6),
  longitude numeric(9,6),
  datetime timestamp without time zone,
  depth numeric(6,2),
  temperature numeric(5,2),
  salinity numeric (5,2),
  ph numeric(5,3),
  alkalinity numeric (6,2),
  dissolved_inorganic_carbon numeric (6,2)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE carbon_chemistry
  OWNER TO lta_team_owner;
"
# sends the command and creates the table
dbGetQuery(con, sql_command)


#example to get data from database
# load the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the database
con <- dbConnect(drv, dbname="LTA_team",
                 host = 'postgres.hi.no', port=5432,
                 user = 'lta_team_owner', password='Noxz3ee!')
rm(pw)


df_postgres <- dbGetQuery(con, "SELECT date, time, salinity, temperature FROM ctd WHERE location ='Leroy_Kaland'")

df_postgres <- dbGetQuery(con, "SELECT * FROM ctd WHERE location='Lysefjord_1km'")

dbDisconnect(con)

dbGetQuery(con, "DELETE FROM ctd WHERE location='Lysefjord_1km'")




## MIGRATION
# local connection
loc <- dbConnect(drv, dbname="postgres",
                 host = 'localhost', port=5432,
                 user = 'LTA_team', password='LTA')


# hi.no connection
con <- dbConnect(drv, dbname="LTA_team",
                 host = 'postgres.hi.no', port=5432,
                 user = 'lta_team_owner', password='Noxz3ee!')

# read local
df_postgres <- dbGetQuery(loc, "SELECT * FROM ctd")

# write to hi.no
dbWriteTable(con, 'ctd', value=df_postgres, append=TRUE, row.names=FALSE)


# get comments and properties of database entries. i.e. to get the units used for each variable.
df_postgres <- dbGetQuery(con, "SELECT c.column_name, c.data_type, pgd.description
                          FROM pg_catalog.pg_statio_all_tables as st inner join
                          pg_catalog.pg_description pgd on (pgd.objoid=st.relid)
                          right outer join information_schema.columns c on
                          (pgd.objsubid=c.ordinal_position and  c.table_schema=st.schemaname
                          and c.table_name=st.relname)
                          where table_schema = 'local' and table_name = 'ctd';")


# updating comments
sql_command <- "COMMENT on column carbon_chemistry.location is 'locality where the data is recorded';
COMMENT on column carbon_chemistry.latitude is 'decimal degrees, latitude of the location';
COMMENT on column carbon_chemistry.longitude is 'decimal degrees, latitude of the location';"

dbGetQuery(con, sql_command)



ctd2=ctd
coordinates(ctd2)=~latitude+longitude





locs <- data.frame(location=as.character(unique(ctd$location)), distance= c(1, 3.9, 5.8, 9, 14.5, 22, 32, 39.5))
ctd2 <- merge(ctd, locs, by='location', all.x=TRUE)
ctd2$r_date <- trunc(ctd2$datetime, unit='day')
ctd2$r_date <- as.POSIXct(ctd2$r_date)
ctd2$date= ctd2$r_date
ctd2$r_date = as.factor(ctd2$date)

library(mgcv)
model <- gam(fluorescence ~ te(distance.y, pressure, by = r_date), data=ctd2)
summary(model)

distance <- seq(min(ctd2$distance.y), max(ctd2$distance.y),0.5)
pressure <- seq(min(ctd2$pressure), max(ctd2$pressure),1)
dates <- unique(ctd2$r_date)

ctd3 <- data.frame(date=rep(dates, each=64428), pressure=rep(pressure, times=1092), distance.y=rep(distance, each=826))
ctd3$r_date <- as.factor(ctd3$date)
ctd3$fluorescence <- predict(model, newdata=ctd3, response='response')
ctd3$fluorescence[which(ctd3$fluorescence>12)] <- 12


flo<-ggplot()+
  geom_tile(data=ctd3, aes(y=-pressure, x=distance.y, fill=fluorescence))+
  scale_fill_gradientn(colours = terrain.colors(10))+
  facet_wrap(~date, ncol=3)

flo

