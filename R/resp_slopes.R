#'@export
#'@import "data.table"
#'@importFrom "MALDIquant" "match.closest"
#'@importFrom "zoo" "rollapplyr"
#'@importFrom "zoo" "zoo"
#'@importFrom "plyr" "summarise"
#'@importFrom "dplyr" "select"
#'@import "ggplot2"
#'@import "RPostgreSQL

resp_slopes <- function (file, ctrl=0, duration=2, interval=4, recalculate='y/n', ctd=NULL, rec_var=c('sal'), humidity=100){
  # This function reads the cvs files from PRESENS new machines used for consecutive close chamber respirometry. It reads the raw data and it converts to DO in mg per litre.
  # Using the information on the interval used it breaks the signal and provides slopes for each respirometry and each channel. Calculates a correction factor from the control channel
  # Recalculations are done from machine phase, and it is only meaningful when external data on salinity or temperature thought more accurate is available. In this case it reads that data from the ctd file.


  a<-read.csv(file, skip=1,  stringsAsFactors = FALSE)
  a<-a[1:nrow(a)-1,]
  a$Date_Time <- strptime(paste(a$Date,' ', a$Time), format = '%m/%d/%Y %H:%M:%S')
  a$Date_Time <- as.POSIXct(a$Date_Time)
  a$delta_t <- as.numeric(as.character(a$delta_t))
  channels <- as.numeric(unique(a$Channel))

  time_lm <- summary(lm(a$delta_t~a$Date_Time))   #checking if the file has a continuous delta_t.
  if (time_lm$r.squared<0.99999){
    for (i in 1:length(channels)){
      a_ch <- subset(a, Channel==i)
      a_ch <- a_ch[order(a_ch$Date_Time),]
      a_ch$delta_t[1]<- 0
      for (j in 2:nrow(a_ch)){
        a_ch$delta_t[j]<-a_ch$delta_t[j-1]+(a_ch$Date_Time[j]-a_ch$Date_Time[j-1])/60
      }
    if (!exists('a2')){
      a2 <- a_ch
    }else{
      a2 <- rbind(a2, a_ch)
    }
    }
    a <- a2
  }


  #recalculation? (y/n)
  # recalculates from phase to current conditions of temperature and salinity. it will require an external dataset with at least salinity to correct.
  if (recalculate=='y'){

    if (is.null(ctd)){
      warning='no external data to recalculate from phase'
    }else{
      if(is.data.frame(ctd)){
          if (min(a$Date_Time)<min(ctd$datetime)|max(a$Date_Time)>max(ctd$datetime)){
            warning='ctd file does not cover the running time of the respirometry file, first and/or last CTD salinity value is used for date/time not covered by the file'
          }
        }
      if(is.character(ctd)){ # that means is a location in the database
            # first check that the location is in the database
            # load the PostgreSQL driver
            drv <- dbDriver("PostgreSQL")
            # creates a connection to the database
            con <- dbConnect(drv, dbname="LTA_team",
                             host = 'postgres.hi.no', port=5432,
                             user = 'lta_team', password='coM7zei.')
            cmd <- paste("SELECT datetime, salinity, temperature FROM ctd WHERE location ='", ctd, "' AND
                       datetime >='", min(a$Date_Time), "' AND datetime <='",max(a$Date_Time),"'",
                         sep='')

            ctd <- dbGetQuery(con, cmd)
      }
      }


    # reading CTD for salinity and temperature.
    ctd <- ctd
    # reducing CTD time coverage to match that of presens file.
    ctd <- subset(ctd, datetime>= min(a$Date_Time) & datetime<=max(a$Date_Time))
    setattr(ctd, "sorted", "datetime")

    clm.sal <- which(colnames(a)=='Salinity')  # search for the variable salinity in the presens file and create it if it isn't there
    if (is.null(clm.sal)){
      a$Salinity=0  # creates the variable salinity
      clm.sal <- which(colnames(a)=='Salinity')
    }

    clm.tem <- which(colnames(a)=='Temp')  # search for the variable temperature in the presens file and create it if it isn't there
    if (is.null(clm.tem)){
      a$Temp=0  # creates the variable salinity
      clm.tem <- which(colnames(a)=='Temp')
    }

    for (i in 1:nrow(a)){
      dt = a[i,ncol(a)]
      y = match.closest(dt, ctd$datetime)
      # salinity
      if (length(grep('sal', rec_var, value=FALSE))>0){
        a[i,clm.sal]<-ctd[y,3]
      }
      # temperature
      if (length(grep('tem', rec_var, value=FALSE))>0){
        a[i,clm.tem]<-ctd[y,5]
      }
    }
    # recalculate oxygen saturation from phase (based on Presens internal data)
    A = tan((a$Cal0+(a$dPhi1*(a$Temp-a$T0)))*pi/180)
    B = 0.0512930699850897+(a$dkSv1*(a$Temp-a$T2nd))
    C = tan(a$Phase*pi/180)

    a$Value=(-(C/A*B+C/A*1/a$m*B-a$f1*1/a$m*B-B+a$f1*B)+
             (sqrt(((C/A*B+C/A*1/a$m*B-a$f1*1/a$m*B-B+a$f1*B)^2)-4*(C/A*1/a$m*(B^2))*(C/A-1))))/
      (2*(C/A*1/a$m*(B^2)))

  }

  # from saturation to mg O2 L. considering salinity, temperature and humidity

  # reduce file to important columns
  out <- data.frame(chnl=a$Channel, delta_t=a$delta_t, Date_Time = a$Date_Time, perc_sat = a$Value, temp = a$Temp, sal = a$Salinity, Bp=a$Pressure/10, ID=a$Sensor_Name)
  out$RH = humidity

  # from saturation to mgO2/l
  sal_factor <- exp(-out$sal * (0.017674-10.754/(out$temp+273.15)^2))
  DO_st_press <- exp(-139.34411+(1.575701*10^5)/(273.15+out$temp)-(6.642308*10^7)/(273.15+out$temp)^2+(1.2438*10^10)/(273.15+out$temp)^3-(8.621949*10^11)/(273.15+out$temp)^4)
  VP_RH <- (10^(8.107131-1730.63/(235+out$temp)))*0.13332239 * out$RH / 100
  Theta <- 0.000975-(1.426*10^-5)*out$temp+(6.436*10^-8)*out$temp^2
  Press_factor <- ((out$Bp/101.325)-(VP_RH/101.325))*(1-Theta*(out$Bp/101.325))/(1-(VP_RH/101.325))*(1-Theta)
  out$DO_mg_l <- Press_factor*DO_st_press*sal_factor*out$perc_sat/100



  # calculating respiration rates
  # splitting by channel
  # which channel is the control? skip it for after this.
  channels <- as.numeric(unique(out$chnl))


  ctrl <- ctrl   # channel for the control
  int <- interval*60 # minutes, interval in hours between start of consecutive respirometries.
  dur <- duration*60 # minutes, time the chambers are closed

  RO <- data.frame(slope=0, t_start=0, t_end=0, R2=0, perc_start=0,
                   perc_end=0, Date_Time=out$Date_Time[1], Temp=0, Sal=0,
                   dur=0, chnl=0, ID="Oxy-0.0")

  for (i in 1:length(channels)){
    if( i != ctrl){
      c1 <- subset(out, chnl==i)
      tmin <- min(c1$delta_t)
      tt<- data.frame(tstart = c(min(c1$delta_t), seq((min(c1$delta_t)+dur),max(c1$delta_t), int)),
                      tend=c(seq((min(c1$delta_t)+dur),max(c1$delta_t), int),max(c1$delta_t)))
      tt <- tt[which((tt$tend-tt$tstart)>=dur),]  # that drops the last one, in case it wasn't complete cycle.
      tt[2:nrow(tt),1] <- tt[2:nrow(tt),1]+(int-dur)  # we know that in each cycle (except the start one) respirometry start after two hours. Cycle is chamber open -- 2 hours -- chamber close -- 2hours
      for (j in 1:nrow(tt)){
          st <- tt[j,'tstart']
          en <- tt[j,'tend']
          if ((en-st)==int){
            st <- st + dur
          }
          ct <- subset(c1, delta_t>=st & delta_t<=en)
          DataExample=data.frame(y=ct$DO_mg_l, t=ct$delta_t)
          Coef <- function(Z) {
            sum <- summary(lm(y ~ t, as.data.frame(Z)))
            return(c(sum$coefficients[2],sum$coefficients[8],sum$r.squared))
          }
          slopes <- rollapplyr(zoo(DataExample), 120, Coef, by.column = FALSE)  # creating rolling slopes on 20 minutes range for the whole dataset
          slopes <- as.data.frame(slopes)
          names(slopes) <- c('slope','p-value', 'R2')
          slopes$t_start <- ct$delta_t[1:(nrow(ct)-119)]
          slopes$t_end <- ct$delta_t[120:nrow(ct)]
          slopes$perc_start <- ct$perc_sat[1:(nrow(ct)-119)]
          slopes$perc_end <- ct$perc_sat[120:nrow(ct)]
          slopes$Date_Time <-ct$Date_Time[1:(nrow(ct)-119)]
          slopes$Temp <- ct$temp[1:(nrow(ct)-119)]
          slopes$Sal <- ct$sal[1:(nrow(ct)-119)]
          slopes<- subset(slopes, slope<=0)
          qual <- quantile(slopes$R2, 0.95)
          slopes <- subset(slopes, R2>qual)

          br <- summarise(.data = slopes,
                        slope  = mean(slope, na.rm=TRUE),
                        R2 = mean(R2, na.rm=TRUE),
                        t_start = min(t_start, na.rm=TRUE) ,
                        t_end =  max(t_end, na.rm=TRUE),
                        perc_start= max(perc_start, na.rm=TRUE),
                        perc_end= min(perc_end, na.rm=TRUE),
                        Date_Time=mean(Date_Time, na.rm=TRUE) ,
                        Temp = mean(Temp, na.rm=TRUE),
                        Sal= mean(Sal, na.rm=TRUE),
                        dur= t_end - t_start)
          br$chnl <- ct$chnl[1]
          br$ID <- ct$ID[1]
          RO <- rbind(RO, br)
          }

    }
  }

  RO<-RO[-1,]
  RO<-subset(RO, !is.na(slope))

  if (ctrl==0){
    warning='no control channel is defined, no control corrections are given'
    RO$ox_cor <- NA   # correction factor from the control
  }else{
    RO$ox_cor <- 0   # correction factor from the control

    ## extracting control values for correction.
    cc <- subset(out, chnl == ctrl)
    for (i in 1:nrow(RO)){   # is up to the researcher to use or not the correction or calculate others, as these may not be subjet to instrument errors.
      st <- RO[i,'t_start']
      en <- RO[i,'t_end']
      ct <- subset(cc, delta_t>=st & delta_t<=en)
      DataExample=data.frame(y=ct$DO_mg_l, t=ct$delta_t)
      Coef <- function(Z) {
        sum <- summary(lm(y ~ t, as.data.frame(Z)))
        return(c(sum$coefficients[2],sum$coefficients[8],sum$r.squared))
      }
      slopes <- rollapplyr(zoo(DataExample), 120, Coef, by.column = FALSE)  # creating rolling slopes on 20 minutes range for the whole dataset
      slopes <- as.data.frame(slopes)
      names(slopes) <- c('slope','p-value', 'R2')
      qual <- quantile(slopes$R2, 0.95)
      slopes <- subset(slopes, R2>qual)
      RO$ox_cor[i] <- median(slopes$slope)
    }
  }

  out <- data.table(out)
  setattr(out, "sorted", "delta_t")
  c1 <- subset(out, chnl==1)
  c1 <- data.table(c1)
  setkey(c1, delta_t)
  brks <-seq(min(c1$delta_t), max(c1$delta_t), 1440)
  lbs =  c1[J(brks), roll="nearest", on='delta_t']
  lbs <- lbs$Date_Time


  RO_plot <- ggplot()+
    geom_line(data=out, aes(x=delta_t, y=perc_sat))+
    geom_segment(data=RO, aes(x = t_start, xend = t_end, y=perc_start, yend = perc_end), colour='red')+
    scale_x_continuous(name='Date - time', breaks=brks, labels=lbs, expand=c(0,0))+
    ylab('%O2 saturation')+
    facet_grid(chnl~.)+
    ggtitle(file) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  dir.create('check_plots')
  pdf(paste('check_plots/', file ,'.pdf', sep=''), width=9, height=6)
  print(RO_plot)
  dev.off()

  R_out <- select(RO, chnl, Date_Time, Temp, Sal, slope, R2, ox_cor, perc_start, perc_end, ID )
  names(R_out) <- c('Channel',  "Date_Time", "Temperature", "Salinity", 'RO' , 'R2', 'RO_ctrl'  ,"satO2_1","satO2_2", 'ID')
  R_out$Date_Time <- as.POSIXct(R_out$Date_Time, origin = "1970-01-01", tz="UTC")
  return(R_out)
}





