# Download the data files and put them in the same directory
# if your are using Rstudio, you can use the Session button from the main menu
# to set the working directory 
# Make sure you have the following libraries installed for the analysis:
#library(lubridate)
#library(openair)
#library(depmixS4)
#library(plyr)
#library(caret)
#library(dplyr)



# Setting the working directory.... IMPORTANT Modify accordingly!

setwd("D:/Dropbox/Occupancy_estimation")
setwd("C:/Users/luism/Dropbox/Occupancy_estimation")


# Run the save and load commands according to your progress. 
# Pay attention to not
# overwrite the file with empty or less data

#load("Occupancyestimation_results_UPDATED_CLEANED.RData")

save.image(file="Occupancyestimation_results_UPDATED_CLEANED.RData")
#write.table(format(energy_data2, digits=19),file = "data_house_temp_humidity.csv", sep = ",", row.names=FALSE)

energy_data3 <- read.csv("data_house_temp_humidity.csv")
energy_data3$date <- strptime(as.character(energy_data3$date),format="%Y-%m-%d %H:%M:%S")
energy_data3$date <- as.POSIXct(energy_data3$date,tz = "UTC")
class(energy_data3)
str(energy_data3)


#all(energy_data3 ==energy_data2) # TRUE
# PERFECT, now the data sets are the same and ready to share online
energy_data2 <-energy_data3

library(lubridate)
library(openair)
library(depmixS4)
library(plyr)
library(caret)


# After running the dominique2 script to analyze temperatures, only interested in 
# the dataaggregated1b data frame
rm(list=setdiff(ls(),"energy_data2"))


# HUMIDITY RATIO CALCULATION

HuRat <- function(Temp,RH){
  Tabs=Temp+273.15
  RH=RH/100;
  p = 1.0133*10^5;
  #Calculating ps (saturated vapor pressure)
  a=-5.8002206*10^3;
  b=1.3914993;
  c=-4.8640239*10^-2;
  d=4.1764768*10^(-5);
  e=-1.4452093*10^-8;
  f=6.5459673;
  ps =exp(a/Tabs+b+(c*Tabs)+d*Tabs^2+e*Tabs^3+f*log(Tabs))
  # Calculating the water vapor pressure
  pv = RH*ps;
  pa = p-pv;
  #Specific humidity/ humidity ratio
  # kg/kg
  W=0.622*pv/pa;
  return(W);
}


# dry air density calculation
density_air_dry <- function(Temp,RH){
  Tabs=Temp+273.15
  RH=RH/100;
  p = 1.0133*10^5;
  #Calculating ps (saturated vapor pressure)
  a=-5.8002206*10^3;
  b=1.3914993;
  c=-4.8640239*10^-2;
  d=4.1764768*10^(-5);
  e=-1.4452093*10^-8;
  f=6.5459673;
  ps =exp(a/Tabs+b+(c*Tabs)+d*Tabs^2+e*Tabs^3+f*log(Tabs))
  # Calculating the water vapor pressure
  pv = RH*ps;
  pa = p-pv;
  #Specific humidity/ humidity ratio
  # kg/kg
  #W=0.622*pv/pa;
  Ra = 287.08756 # Joule/kg-K
  d_air = pa/(Ra*Tabs)
  
  return(d_air);
}





energy_data2$den_3 <- density_air_dry(energy_data2$T3,energy_data2$RH_3)




#View(energy_data)

# Exploratory analysis 


par(mfrow=c(2,1))
boxplot(energy_data2$RH_1 ~ as.POSIXlt(energy_data2$date)$hour)
boxplot(energy_data2$W_1 ~ as.POSIXlt(energy_data2$date)$hour)


par(mfrow=c(2,1))
boxplot(energy_data2$RH_8 ~ as.POSIXlt(energy_data2$date)$hour)
boxplot(energy_data2$W_8 ~ as.POSIXlt(energy_data2$date)$hour)
dev.off()


par(mfrow=c(2,1))
boxplot(energy_data2$RH_9 ~ as.POSIXlt(energy_data2$date)$hour)
boxplot(energy_data2$W_9 ~ as.POSIXlt(energy_data2$date)$hour)
dev.off()

par(mfrow=c(2,1))
boxplot(energy_data2$RH_3 ~ as.POSIXlt(energy_data2$date)$hour)
boxplot(energy_data2$W_3 ~ as.POSIXlt(energy_data2$date)$hour)
dev.off()



# Creating functions for inferring the occupancy

check_pred <- function(x,predictions_DF){
  # x is  data frame with diff moisture
  
  if (mean(x[predictions_DF[,1]==1]) < mean(x[predictions_DF[,1]==2])
     & max(x[predictions_DF[,1]==2]) > max(x[predictions_DF[,1]==1] )
     )
  
    {
    print('perfect')
    inverted <- predictions_DF[,1]
  } else{
    print('need to invert the prediction')
    n <- length(predictions_DF[,1])
    inverted <- vector()
    for (i in 1:n) {
      if (predictions_DF[,1][i] == 1) {
        val2 = 2
        inverted <- c(inverted,val2)
      }
      else {
        val2= 1
        inverted <- c(inverted,val2)
      }
      
    }
    
  }
  return(inverted)
}








Relevel_0_1 <- function(x) {
  
  if (x == 2) {
    val2 = 1
  }
  else {
    val2= 0
  }
  return(val2)
}


HMM_Performance <- function(mydata_W,mydata_date){
  W3  <- c(mydata_W)
  mydata_date
  W3s <- W3
  dW3 <- diff(W3s)
  
  mod3 <- depmix(list(dW3 ~ 1),
                 data=data.frame(dW3),
                 family=list(gaussian()),
                 nstates=2)
  f3 <- fit(mod3)
  n <- length(dW3)+1
  n
  esttrans_1_day <- posterior(f3)
  esttrans_1_day[,1] <- check_pred(dW3,esttrans_1_day)
  
  occup1 <- unlist(lapply(esttrans_1_day[,1],Relevel_0_1 ))
  
  
  o <- par(no.readonly=T)
  par(mfrow=c(2,1))
  plot(mydata_date, c(dW3[1],dW3) , type='l', main='Humidity Ratio Difference',
       xlab = "Time",ylab="Humidity Ratio Difference")
  plot(mydata_date, c(  occup1[1],  occup1), type='l',lwd=c(0.1), main='Estimated state',
       xlab = "Time", ylab="State",col='black',ylim=c(0,1.4), yaxt="n")
  #lines(mydata_date, c(  occup1[1],  occup1),col='red')
  legend("topright", inset=c(0,0),c("Estimated"), lty=c(1), 
         lwd=c(1),col=c("black"),
         bg="transparent",cex=0.75,bty = "n")
  axis(2, at=c(0,1),labels=c(0,1), col.axis="black", las=1)
  
 #reconfu <-confusionMatrix(mresu$Occupancy,c(occup1[1],occup1),positive = "0")
  #reconfu
  return(occup1)
}



#mresu <- Comp_time_mean_adjust_occup("10 min",datatraining)
library(depmixS4)

summary(energy_data2)
#View(energy_data2)









#library(timeSeries)

#dim(energy_data2)

#energy_data2[,2:28] <- interpNA(energy_data2[,2:28],method="linear")

#summary(energy_data2)

## now that is clean...
#24*6



set.seed(1)
HMM_Performance(energy_data2$W_1,energy_data2$date)





head(energy_data2$date)
tail(energy_data2$date)




Invert_0_1 <- function(x) {
  
  if (x == 1) {
    val2 = 0
  }
  if (x == 0) {
    val2= 1
  }
  return(val2)
}





library(dplyr)


set.seed(1)
occup1  <-   HMM_Performance(energy_data2$W_1,energy_data2$date)


DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))

#View(DF_OC1)

library(lubridate)

DF_OC1$hour <- hour(DF_OC1$date)

#View(DF_OC1)


DF_OC1_by_hour <- group_by(DF_OC1,hour)

mysummary <-summarise(DF_OC1_by_hour, 
                      mean= mean(occupancy),
                      sd = sd(occupancy))

#View(mysummary )

weekend_weekday <- function(x) {
  val <- weekdays(x)
  if (val == "Saturday" | val == "Sunday") {
    val2 = "Weekend"
  }
  else {
    val2= "Weekday"
  }
  return(val2)
}

day_week <- function(x) {
  val <- weekdays(x)
  
  return(val)
}




DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
#dim(DF_OC1)


DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))


DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)

#View(DF_OC1_status_1)

DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
#View(DF_OC1_status_WS)


DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                         levels = c("Monday", "Tuesday", "Wednesday",
                                    "Thursday", "Friday", "Saturday", "Sunday"))

Room1_Kitchen <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
  geom_line(size=1) + coord_cartesian(ylim = c(0, 1)) +
  labs(y='Occupancy',x='hour',colour='Week Day')+ 
  scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
  theme(panel.grid.major = element_line(colour = "gray"))

Room1_Kitchen 


# For W2

set.seed(1)
occup1  <-   HMM_Performance(energy_data2$W_2,energy_data2$date)

DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))

#View(DF_OC1)

DF_OC1$hour <- hour(DF_OC1$date)

#View(DF_OC1)


DF_OC1_by_hour <- group_by(DF_OC1,hour)

mysummary <-summarise(DF_OC1_by_hour, 
                      mean= mean(occupancy),
                      sd = sd(occupancy))


DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))

DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))


DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)

#View(DF_OC1_status_1)

DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
#View(DF_OC1_status_WS)


# Best

ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
  geom_line(size=1) +
  labs(y='Occupancy',x='hour',colour='Day_week')+ 
  scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2)

DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                     levels = c("Monday", "Tuesday", "Wednesday",
                                                "Thursday", "Friday", "Saturday", "Sunday"))

 Room2  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
    geom_line(size=1) + coord_cartesian(ylim = c(0, 1)) +
  labs(y='Occupancy',x='hour',colour='Week Day')+ 
  scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
  theme(panel.grid.major = element_line(colour = "gray"))

 Room2 
 
 # For W9 (parents room)
 
 

 
 set.seed(1)
 occup1  <-   HMM_Performance(energy_data2$W_9,energy_data2$date)
 
 #inverted
 #occup1 <- unlist(lapply(occup1,Invert_0_1))
 
 DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
 #View(DF_OC1)
 
 DF_OC1$hour <- hour(DF_OC1$date)
 
 #View(DF_OC1)
 
 
 DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
 mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 
 DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
 DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
 DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
 # 
 head(DF_OC1)

 
 #View(DF_OC1_status_1)
 
 DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
 head(DF_OC1_status_WS)
 #View(DF_OC1_status_WS)
 
  # Best
 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room9  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 Room9  
 
 # For Room 3
 
# HMM_Performance(energy_data$W_3,energy_data$date)
set.seed(1)
occup1  <-   HMM_Performance(energy_data2$W_3,energy_data2$date)
 #inverted
DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
#View(DF_OC1)
 
DF_OC1$hour <- hour(DF_OC1$date)
 
#View(DF_OC1)
 
 
DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 
DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
#View(DF_OC1_status_1)
 
DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
# View(DF_OC1_status_WS)
 
 
 # Best

 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room3_not_inverted  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 
 Room3_not_inverted
 
 
 
 
 # Room 8 without inversion

 # HMM_Performance(energy_data$W_3,energy_data$date)
 set.seed(1)
 occup1  <-   HMM_Performance(energy_data2$W_8,energy_data2$date)

 
 DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
 #View(DF_OC1)
 
 DF_OC1$hour <- hour(DF_OC1$date)
 
 #View(DF_OC1)
 
 
 DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
 mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 
 weekend_weekday <- function(x) {
   val <- weekdays(x)
   if (val == "Saturday" | val == "Sunday") {
     val2 = "Weekend"
   }
   else {
     val2= "Weekday"
   }
   return(val2)
 }
 
 day_week <- function(x) {
   val <- weekdays(x)
   
   return(val)
 }
 
 
 
 
 DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
 DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
 DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
 #View(DF_OC1_status_1)
 
 DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
 #View(DF_OC1_status_WS)
 
 
 # Best
 
 ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +
   labs(y='Occupancy',x='hour',colour='Day_week')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2)
 
 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room8_notinverted  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 
 Room8_notinverted 
 
 
 
 # Room 8
 # HMM_Performance(energy_data$W_3,energy_data$date)
 set.seed(1)
 occup1  <-   HMM_Performance(energy_data2$W_8,energy_data2$date)
 #inverted
 occup1 <- unlist(lapply(occup1,Invert_0_1))
 
 DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
 #View(DF_OC1)
 
 DF_OC1$hour <- hour(DF_OC1$date)
 
 #View(DF_OC1)
 
 
 DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
 mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 
 DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
 DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
 DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
 #View(DF_OC1_status_1)
 
 DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
 #View(DF_OC1_status_WS)
 
 
 # Best
 
 ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +
   labs(y='Occupancy',x='hour',colour='Day_week')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2)
 
 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room8  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 
 Room8
 

 
 
 
 
 # Room 7
 
 # Room 7
 # HMM_Performance(energy_data$W_3,energy_data$date)
 set.seed(1)
 occup1  <-   HMM_Performance(energy_data2$W_7,energy_data2$date)
 #inverted
 #occup1 <- unlist(lapply(occup1,Invert_0_1))
 
 DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
 #View(DF_OC1)
 
 DF_OC1$hour <- hour(DF_OC1$date)
 
 #View(DF_OC1)
 
 
 DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
 mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
 DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
 DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
 #View(DF_OC1_status_1)
 
 DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
 #View(DF_OC1_status_WS)
 
 
 # Best
 

 
 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room7  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 
 Room7
 
 
 #
 
 # Room 5
 # HMM_Performance(energy_data$W_3,energy_data$date)
 set.seed(1)
 occup1  <-   HMM_Performance(energy_data2$W_5,energy_data2$date)
 

 
 DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
 #View(DF_OC1)
 
 DF_OC1$hour <- hour(DF_OC1$date)
 
 #View(DF_OC1)
 
 
 DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
 mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 
 DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
 DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
 DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
 #View(DF_OC1_status_1)
 
 DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
 #View(DF_OC1_status_WS)
 
 
 # Best
 
 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room5  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 
 Room5
 
 

 # Room 4
 
 # Room 4
 # HMM_Performance(energy_data$W_3,energy_data$date)
 set.seed(1)
 occup1  <-   HMM_Performance(energy_data2$W_4,energy_data2$date)
 #plot(energy_data2$date,c(occup1[1],occup1))
 
 
 DF_OC1  <- data.frame(date=energy_data2$date,occupancy=c(occup1[1],occup1))
 
 #View(DF_OC1)
 
 DF_OC1$hour <- hour(DF_OC1$date)
 
 #View(DF_OC1)
 
 
 DF_OC1_by_hour <- group_by(DF_OC1,hour)
 
 mysummary <-summarise(DF_OC1_by_hour, 
                       mean= mean(occupancy),
                       sd = sd(occupancy))
 
 
 DF_OC1$WeekStatus <-unlist(lapply(DF_OC1$date,weekend_weekday))
 
 DF_OC1$day_week2 <- unlist(lapply(DF_OC1$date,day_week))
 
 
 DF_OC1_status_1 <- group_by(DF_OC1,day_week2,hour)
 
 #View(DF_OC1_status_1)
 
 DF_OC1_status_WS <- dplyr::summarise(DF_OC1_status_1,mean=mean(occupancy))
 #View(DF_OC1_status_WS)
 
 
 # Best
 
 
 DF_OC1_status_WS$day_week2 <- factor(DF_OC1_status_WS$day_week2, 
                                      levels = c("Monday", "Tuesday", "Wednesday",
                                                 "Thursday", "Friday", "Saturday", "Sunday"))
 
 Room4  <- ggplot(DF_OC1_status_WS, aes(x=hour,y=mean,colour=day_week2)) +
   geom_line(size=1) +  coord_cartesian(ylim = c(0, 1)) +
   labs(y='Occupancy',x='hour',colour='Week Day')+ 
   scale_x_continuous(breaks = 0:23)+facet_wrap(~ day_week2,scales="free")+
   theme(panel.grid.major = element_line(colour = "gray"))
 
 Room4
 
 
head(energy_data2$date)
#"2016-09-04 00:00:00 UTC
tail(energy_data2$date) 
# "2016-10-15 23:50:00 UTC"

## RUN THIS LINES IF YOU WANT TO SAVE THE PLOTS AS PNG FILES....

#ggsave("Room_1_kitchen2_sept04_oct17_new_range.png",Room1_Kitchen,width = 13.5,height=10)
 
#ggsave("Room_2_Living_room_sept04_oct17_new_range.png",Room2,width = 13.5,height=10)

#ggsave("Room_3NOTINVERTED_Laundry_room_sept04_oct17_new_range.png",Room3_not_inverted,width = 13.5,height=10)

#ggsave("Room_4_Office_room_sept04_oct17_new_range.png",Room4,width = 13.5,height=10)  
#ggsave("Room_5_Shower_room_sept04_oct17_new_range.png",Room5,width = 13.5,height=10)  
#ggsave("Room_7_Ironing_room_sept04_oct17_new_range.png",Room7,width = 13.5,height=10)  
#ggsave("Room_8_Teenager_B_room_sept04_oct17_new_range.png",Room8,width = 13.5,height=10)
#ggsave("Room_8NOTINVERTED_Teenager_B_room_sept04_oct17_new_range.png",Room8_notinverted,width = 13.5,height=10)
#ggsave("Room_9_Parents_room_sept04_oct17_new_range.png",Room9,width = 13.5,height=10)  



# Time difference
tdiff <- head(energy_data2$date,1)-tail(energy_data2$date,1)
tdiff

# 43.993 days
# appprox 43.993 days - 4 days of absence
# 40 days of data approximate
as.period(tdiff+as.duration(days(4)),unit="days")

#"-39d -23H -50M 0S"


# Moisture content between 0 and 5 hr -------------------------------------
energy_data3 <-energy_data2
summary(energy_data3)
energy_data3$dayyear <- yday(energy_data3$date)
#View(energy_data3)

energy_data3_splitted <- split(energy_data3,energy_data3$dayyear)

length(energy_data3_splitted)
energy_data3_splitted[[1]]
#View(energy_data3_splitted[[1]])
#View(energy_data3_splitted[[2]])

# energy_data3_splitted[[1]]$date[31]  for 5 a.m.
#energy_data3_splitted[[1]]$date[1] for midnight 0.00


mylist <- vector()
for (i in seq(1,length(energy_data3_splitted))){
  print(i)
  #print(energy_data3_splitted[[i]]$date[31])
  #print(energy_data3_splitted[[i]]$date[1])
  value <- (energy_data3_splitted[[i]]$W_3[31]*energy_data3_splitted[[i]]$den_3[31]-
              energy_data3_splitted[[i]]$W_3[1]*energy_data3_splitted[[i]]$den_3[1])*1000
  mylist <- c(mylist,value)
}
length(mylist)

mean(mylist)*(43.12/5)
# 4.7811 g/h average laundry room m3 = 43.12


mean(mylist[mylist>=0])*(43.12/5)

# 7.08891 g/h average laundry room m3 = 43.12


max(mylist[mylist>=0])*(43.12/5)
# 14.95712

min(mylist[mylist>=0])*(43.12/5)
# 0.4605



plot(mylist)





















