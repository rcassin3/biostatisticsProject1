install.packages("jsonlite", repos="https://cran.rstudio.com/")
install.packages("ggplot2")
library("jsonlite")
library(readxl)
library(tcltk)
library(tidyverse)
library(dplyr)
library(ggplot2)
if(!require(psych)){install.packages("psych")}
if(!require(mnormt)){install.packages("mnormt")}
library(stringr)

json_fileseaLevel <- 'https://datahub.io/core/sea-level-rise/datapackage.json'
json_dataseaLevel <- fromJSON(paste(readLines(json_fileseaLevel), collapse=""))

# get list of all resources:
print(json_dataseaLevel$resources$name)

# print all tabular data(if exists any)
for(i in 1:(length(json_dataseaLevel$resources$datahub$type)-1)){
  if(json_dataseaLevel$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_dataseaLevel$resources$path[i]
    dataSea <- read.csv(url(path_to_file))
    print(dataSea)
  }
}

df2 <- select (dataSea, c('Year', 'CSIRO.Adjusted.Sea.Level'))
df2 <- df2 %>%
  mutate_at("Year", str_replace, "-03-15", "") 
na.omit(df2)
windows()
ggplot( na.omit(df2), aes(x = Year,y = CSIRO.Adjusted.Sea.Level, group = 1) ) + geom_line( color = "red", linetype = "solid",  size = 1)

windows()
plot(density(unlist(na.omit(df2[2]))), main = "Density plot of Sea Levels from 1880-2013")
windows()
qqnorm(unlist(na.omit(df2[2])), main = "QQ plot of Sea Levels from 1880-2013")
qqline(unlist(na.omit(df2[2])))
print(shapiro.test( as.numeric( na.omit(df2$CSIRO.Adjusted.Sea.Level) ) )) # Use the p-value returned by the normality test

yearsAdjusted2 <- subset(df2, df2$Year <= 2010)
yearsAdjusted2[] <- lapply(yearsAdjusted2, function(x) as.numeric(as.character(x)))
windows()
ggplot( yearsAdjusted2, aes(x = Year,y = CSIRO.Adjusted.Sea.Level, group = 1) ) + geom_line( color = "red", linetype = "solid",  size = 1)

windows()
plot(density(unlist(yearsAdjusted2[2])), main = "Density plot of Cumulative Sea Levels 1880-2010")
windows()
qqnorm(unlist(yearsAdjusted2[2]), main = "QQ plot of Cumulative Sea Level Change 1880-2010")
qqline(unlist(yearsAdjusted2[2]))
print(shapiro.test( as.numeric( yearsAdjusted2$CSIRO.Adjusted.Sea.Level ) )) # Use the p-value returned by the normality test

