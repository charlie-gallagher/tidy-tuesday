#########################################################
#                                                       #
#      Hotel booking demand datasets exploration        #
#                                                       #
#########################################################

#########################################################
#             WORKSPACE PREPARATION                     #
#########################################################
# Clean workspace
rm(list=ls())

## Load libraries
library(skimr)
library(tabplot)

## Load datasets
bookingsH1=read.csv("H1.csv",sep=",",stringsAsFactors = FALSE, na.strings = "NULL")
bookingsH2=read.csv("H2.csv",sep=",",stringsAsFactors = FALSE, na.strings = "NULL")

## Convert to factors
toFactors<-c("Agent","ArrivalDateMonth","AssignedRoomType","Company","Country","CustomerType","DepositType",
           "DistributionChannel","IsCanceled","IsRepeatedGuest","MarketSegment","Meal","ReservationStatus",
           "ReservedRoomType")
# H1
bookingsH1[,toFactors]<-lapply(bookingsH1[,toFactors],factor)
# H2
bookingsH2[,toFactors]<-lapply(bookingsH2[,toFactors],factor)

## Convert to date
# H1
bookingsH1$ReservationStatusDate<-as.Date(bookingsH1$ReservationStatusDate)
# H2
bookingsH2$ReservationStatusDate<-as.Date(bookingsH2$ReservationStatusDate)

## Convert to numeric
# H2
bookingsH2$Children<-as.integer(bookingsH2$Children)


## Save R Objects
# H1
#saveRDS(bookingsH1, file="H1.rds")
# H2
#saveRDS(bookingsH2, file="H2.rds")

## Summary Statistics
# H1
summary(bookingsH1)
skim(bookingsH1)
head(summary(bookingsH1$Agent),5)
head(summary(bookingsH1$Company),5)

# H2
summary(bookingsH2)
skim(bookingsH2)
head(summary(bookingsH2$Agent),5)
head(summary(bookingsH2$Company),5)


## Table plots
bookingClassColor <- c("#999999", "#FF4C4C")
# H1 
tabH1<-tableplot(bookingsH1,
          select=c(IsCanceled, Adults, Children, Babies, StaysInWeekendNights, StaysInWeekNights, Meal, Country, ReservedRoomType, AssignedRoomType),
          pals=list(IsCanceled = bookingClassColor),
          scales="lin", numMode = "MB-ML"
          )
#tableSave(tabH1, filename="Figure2.tiff", width=12, height = 8)

# H2  
tabH2<-tableplot(bookingsH2,
          select=c(IsCanceled, Adults, Children, Babies, StaysInWeekendNights, StaysInWeekNights, Meal, Country, ReservedRoomType, AssignedRoomType),
          pals=list(IsCanceled = bookingClassColor),
          scales="lin", numMode = "MB-ML"
          )
#tableSave(tabH2, filename="Figure3.tiff", width=12, height = 8)




