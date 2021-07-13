#This file processes the sales data from the assessor data
#Jeremy R. Groves
#Created: July 5, 2021

rm(list=ls())
library(foreign)
library(tidyverse)

clean<-function(x){
x<- x %>%
    mutate(SALEDT = as.character(SALEDT)) %>%
      mutate(SALEYR = substr(SALEDT,nchar(SALEDT)-3,nchar(SALEDT))) %>%
        mutate(SALEDY = substr(SALEDT, nchar(SALEDT)-5,nchar(SALEDT)-4)) %>%
          mutate(SALEMN = substr(SALEDT, 1, nchar(SALEDT)-6)) %>%
            mutate(SALEDT = as.Date(paste(SALEMN,SALEDY,SALEYR,sep='/'), "%m/%d/%Y")) %>%
              select(PARID,SALEDT,PRICE,SALETYPE,SALEVAL) %>%
                subset(!is.na(PRICE)) %>%
                  subset(SALEVAL=="X") %>%
                    arrange(PARID,SALEDT) %>%
                      distinct(PARID,SALEDT,PRICE,.keep_all = TRUE)
return(x)
}
  
clean2<-function(x){
  x<- x %>%
    mutate(SALEDT = as.Date(SALEDT, "%m/%d/%Y")) %>%
     select(PARID,SALEDT,PRICE,SALETYPE,SALEVAL) %>%
        subset(!is.na(PRICE)) %>%
          subset(SALEVAL=="X") %>%
            arrange(PARID,SALEDT) %>%
              distinct(PARID,SALEDT,PRICE,.keep_all = TRUE)
  return(x)
}

clean3<-function(x){
  x<- x %>%
    mutate(SALEDT = as.Date(SALEDT, "%d-%b-%y")) %>%
    select(PARID,SALEDT,PRICE,SALETYPE,SALEVAL) %>%
    subset(!is.na(PRICE)) %>%
    subset(SALEVAL=="X") %>%
    arrange(PARID,SALEDT) %>%
    distinct(PARID,SALEDT,PRICE,.keep_all = TRUE)
  return(x)
}

sales<-read.csv("./Build/Input/sales01.csv",as.is=TRUE,header=TRUE)
sales<-clean(sales)


b<-c("02","03","04","05","06","07","08")
for(i in b){

  sales2<-read.csv(paste0("./Build/Input/sales",i,".csv"),as.is=TRUE,header=TRUE)
  sales2<-clean(sales2)
  
  sales2<-anti_join(sales2,sales) #Pulls unique values from sales2
  print(c(i, nrow(sales2)))
  sales<-rbind(sales,sales2) #Adds unique values to core sales data
    rm(sales2) #Removes the sales2 data for next iteration
}

b<-c("09","10")
for(i in b){
  
  sales2<-read.csv(paste0("./Build/Input/sales",i,".csv"),as.is=TRUE,header=TRUE)
  sales2<-clean2(sales2)
  
  sales2<-anti_join(sales2,sales) #Pulls unique values from sales2
  print(c(i, nrow(sales2)))
  sales<-rbind(sales,sales2) #Adds unique values to core sales data
  rm(sales2) #Removes the sales2 data for next iteration
}

b<-c("17","18")
for(i in b){
  
  sales2<-read.csv(paste0("./Build/Input/sales",i,".csv"),as.is=TRUE,header=TRUE)
  sales2<-clean3(sales2)
  
  sales2<-anti_join(sales2,sales) #Pulls unique values from sales2
  print(c(i, nrow(sales2)))
  sales<-rbind(sales,sales2) #Adds unique values to core sales data
  rm(sales2) #Removes the sales2 data for next iteration
}

save(sales,file="./Build/OUtput/Master Sales.RData")
