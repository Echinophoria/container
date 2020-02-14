# IMPORTING AND GENERATING RESULTS FROM PAMAS counters

### Importing data from PAMAS raw txt files

setwd("G:/pamas/results_container_2020_01_20_to_2020_01_29")  # directory where PAMAS files are stored in RStudio it can be searched and stablished using the explorer
files<-dir()  # reading files in directory, make sure only PAMAS files are present in the directory. Or choose the files manually

out=NULL

for (i in 1:length(files)){
  counts <- read_PAMAS(files[i]) # read the pamas files and return them organised in a data frame. Doesn't do anything to the raw data.
  if (is.null(out)){
    out = counts
  }else{
    counts$block <- counts$block + max(out$block)  # to avoid replication of block numbers
    out = rbind(out, counts)
  }
}

# cleaning duplicates. Different files may contain same measurements
out <- unique(out)  
# removing empty lines, there are only 12 replicates at each block
out <- subset(out, chamber_ID<=12) 

## generating results
## clearance rates
# load time series of chamber flow rates
flow_rate <- read.table("G:/pamas/Flow_rates_container.txt", sep='\t', header=TRUE)
flow_rate[,2] <- as.POSIXct(strptime(paste(flow_rate[,1],' ', flow_rate[,2]), format = '%d.%m.%Y %H:%M:%S'))
colnames(flow_rate)[2]<-'Date_Time'

# make sure it covers the time frame for the measurements
out <- subset(out, Date_Time>=flow_rate[1,2] & Date_Time<=flow_rate[nrow(flow_rate),2])

# clean file of incomplete measurements, it happens when measurements are canceled by user or other reason.
blocks <- unique(out$block)
out_clean <- NULL
for (i in 1:length(blocks)){
  oo <- subset(out, block==blocks[i])
  if (is.null(out_clean)){
    if (nrow(oo)<12){ # this case is for 12 measurements
      out_clean <- NULL
    }else{
    out_clean <- oo
    }
  }else{
    if (nrow(oo)<12){ # this case is for 12 measurements
      out_clean <- out_clean
    }else{
      out_clean <- rbind(out_clean,oo)
    }
  }
}
remove(oo, counts)

clearance <- get_CR(out_clean, c(1,12), flow_rate, ranges=NULL)

input_vol <- get_volumes(clearance[[1]])   # getting volumes of input particles by size range


# example for plotting and reorganising data
library(tidyr)
library(plyr)
data <- pivot_longer(input_vol, cols=6:37, names_to = 'particle_size') # table long format 
short<- ddply(.data = data, c('Date_Time',"particle_size"),summarise,
               N    = length(value),
               Mean = mean(value, na.rm=TRUE),
               sd = sd(value, na.rm=TRUE),
               se = sd/sqrt(N))
short<- subset(short, particle_size!='chamber_ID.1')
short$nm <- as.numeric(sub(">.*","", short$particle_size))

library(ggplot2)
in_vls <- ggplot()+
  geom_line(data=short, aes(x=nm, y=Mean))+
  geom_ribbon(data=short, aes(x=nm, ymin=Mean-sd, ymax=Mean+sd), alpha=0.2)+
  scale_x_continuous(breaks=short$nm, labels=short$particle_size)+
  facet_grid(Date_Time~.)

in_vls

