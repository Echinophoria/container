setwd("G:/Respirometry")
a<-read.csv('Tank_1_O2-13Jan2020_23Jan2020.csv', skip=1)
a<-a[1:nrow(a)-1,]
a$Date_Time <- strptime(paste(a$Date,' ', a$Time), format = '%m/%d/%Y %H:%M:%S')
a$Date_Time <- as.POSIXct(a$Date_Time)
a$delta_t <- as.numeric(levels(a$delta_t))[a$delta_t]
a <- subset(a, delta_t>=10000) # 12793
a#recalculation? (y/n)
# if yes further data on salinity, temperature, humidity, etc must be given

# reading CTD for salinity and temperature 
library(data.table)
ctd<-read.table('CTD_Austevooll_13_01_2020-22_01_2020.txt', skip=7, sep=';', header=TRUE)
ctd$Date_Time <- strptime(paste(ctd$Date,' ', ctd$Time), format = '%d.%m.%Y %H:%M:%S')
ctd$Date_Time <- as.POSIXct(ctd$Date_Time)
ctd=data.table(ctd) # to make it searchable

clm.sal <- which(colnames(a)=='Salinity') 
if (is.null(clm.sal)){
  a$Salinity=0  # creates the variable salinity
  clm.sal <- which(colnames(a)=='Salinity')
}

clm.tem <- which(colnames(a)=='Temp')
if (is.null(clm.tem)){
  a$Temp=0  # creates the variable salinity
  clm.tem <- which(colnames(a)=='Temp')
}

for (i in 1:nrow(a)){
  x = a[i,ncol(a)]
  y = ctd[J(x), roll="nearest", on='Date_Time']
  # salinity
  a[i,clm.sal]<-y[1,3]
}

# re-calculating oxygen saturation from phase.
# on hold missing one parameter.Internal calculation may be done different to current excel 

# reduce file to important columns
out <- data.frame(chnl=a$Channel, delta_t=a$delta_t, Date_Time = a$Date_Time, perc_sat = a$Value, temp = a$Temp, sal = a$Salinity, Bp=a$Pressure/10)
out$RH = 100
# from saturation to mmol/l
    sal_factor <- exp(-out$sal * (0.017674-10.754/(out$temp+273.15)^2))
    DO_st_press <- exp(-139.34411+(1.575701*10^5)/(273.15+out$temp)-(6.642308*10^7)/(273.15+out$temp)^2+(1.2438*10^10)/(273.15+out$temp)^3-(8.621949*10^11)/(273.15+out$temp)^4)
      VP_RH <- (10^(8.107131-1730.63/(235+out$temp)))*0.13332239 * out$RH / 100
      Theta <- 0.000975-(1.426*10^-5)*out$temp+(6.436*10^-8)*out$temp^2
    Press_factor <- ((out$Bp/101.325)-(VP_RH/101.325))*(1-Theta*(out$Bp/101.325))/(1-(VP_RH/101.325))*(1-Theta)
out$DO_mg_l <- Press_factor*DO_st_press*sal_factor*out$perc_sat/100


# splitting by channel
# which channel is the control? skip it for after this.
library(zoo)
channels <- as.numeric(unique(out$chnl))
ctrl <- 4   # channel for the control
RQ <- data.frame(t_delta_1=0, t_delta_2 = 0, RQ=0, Temperature=0, Salinity=0, Date_Time = 0,chnl=0)
for (j in 1:length(channels)){
  if (j != ctrl){
    c1 <- subset(out, chnl==j)
    
    y=c1$DO_mg_l
    t=c1$delta_t
    DataExample=data.frame(y=y, t=t)
    Coef <- function(Z) {
      sum <- summary(lm(y ~ t, as.data.frame(Z)))
      return(c(sum$coefficients[2],sum$r.squared))
    }
    slopes <- rollapplyr(zoo(DataExample), 120, Coef, by.column = FALSE)  # creating rolling slopes on 20 minutes range for the whole dataset
    slopes <- as.data.frame(slopes)
    names(slopes) <- c('slope', 'R2')
    slopes$start <- c1$delta_t[1:(nrow(c1)-119)]
    slopes$end <- c1$delta_t[120:nrow(c1)]
    qual <- quantile(slopes$R2, 0.95)
    slopes <- subset(slopes, R2>=qual)
    slopes$gap <- c(0, slopes$start[2:nrow(slopes)] - slopes$start[1:(nrow(slopes)-1)])
    index <- c(0,which(slopes$gap > 0.18))
    R <- data.frame(t_delta_1=seq(1,length(index)-1,1), t_delta_2=0, RQ = 0)
    for (i in 1:(length(index)-1)){
      strow <- index[i]+1
      ndrow<- index[i+1]-1
      rr<- slopes[strow:ndrow,]
      R[i,3] <- mean(rr$slope)
      R[i,1] <- rr[1,3]
      R[i,2] <- rr[nrow(rr),4]
    }
    
    R$Temperature <- 0
    R$Salinity <- 0
    R$Date_Time <- 0
    
    
    for (i in 1:nrow(R)){
      t1 <- R[i,1]
      t2 <- R[i,2]
      mn <- subset(c1, delta_t>=t1 & delta_t<=t2)
      R[i,4]<-mean(mn$temp)
      R[i,5]<-mean(mn$sal)
      R[i,6]<-mn[1,ncol(c1)]
    }
    R$chnl=j
    RQ = rbind(RQ, R)
  }
}
RQ<-RQ[-1,]

library(ggplot2)

RO_plot <- ggplot()+
  geom_line(data=out, aes(x=delta_t, y=DO_mg_l))+
  geom_rect(data=RQ, inherit.aes=FALSE,
            aes(xmin=t_delta_1,xmax=t_delta_2,ymin=5.5,ymax=7),
                fill = 'grey50', alpha=0.2)+
  facet_grid(chnl~.)

RO_plot



## extracting control values for correction.
cc <- subset(out, chnl == ctrl)


ggplot()


