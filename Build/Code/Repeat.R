rm(list=ls())
library(foreign)
library(tidyverse)


load("./Build/Output/Master Sales.RData")

#Removes very low dollar amounts and takes the most recent of repeated sales observations
sale<-sales %>%
  subset(PRICE>19999)%>%
  arrange(PARID,SALEDT,PRICE) %>%
    group_by(PARID,SALEDT) %>%
      filter(!is.na(SALETYPE)) %>%
      mutate(ticker=row_number()) %>%
        mutate(num = max(ticker)) %>%
          filter(ticker==num) %>%
            select(PARID, SALEDT, PRICE, SALETYPE, SALEVAL)

#Pulls out PARIDs with more than one sale present in data
temp<-sale %>%
      group_by(PARID) %>%
        count(PARID) %>%
          subset(n>1) %>%
            distinct(PARID)

rep<-merge(temp,sale,by="PARID",all.x=TRUE) #All Repeat Sales in Data