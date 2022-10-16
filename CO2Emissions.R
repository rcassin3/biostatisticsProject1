install.packages("jsonlite", repos="https://cran.rstudio.com/")
install.packages("ggplot2")
library("jsonlite")
library(readxl)
library(tcltk)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggpubr)

if(!require(psych)){install.packages("psych")}
if(!require(mnormt)){install.packages("mnormt")}

json_fileCO2 <- 'https://datahub.io/core/co2-fossil-global/datapackage.json'
json_dataCO2 <- fromJSON(paste(readLines(json_fileCO2), collapse=""))

# get list of all resources:
print(json_dataCO2$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_dataCO2$resources$datahub$type)){
  if(json_dataCO2$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_dataCO2$resources$path[i]
    dataCO2 <- read.csv(url(path_to_file))
    print(dataCO2)
  }
}

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


df <- select (dataCO2, c('Year', 'Total'))
windows()
ggplot( df, aes(x = Year,y = Total) ) + geom_line( color = "blue", linetype = "solid",  size = 1)


df2 <- select (dataSea, c('Year', 'CSIRO.Adjusted.Sea.Level'))
df2 <- df2 %>%
  mutate_at("Year", str_replace, "-03-15", "") 
yearsAdjusted2 <- subset(df2, df2$Year <= 2010)
yearsAdjusted2[] <- lapply(yearsAdjusted2, function(x) as.numeric(as.character(x)))


# Check normality of one column
windows()
plot(density(unlist(df[2])), main = "Density plot of Test 1")
windows()
qqnorm(unlist(df[2]), main = "QQ plot of Test 1")
qqline(unlist(df[2]))
print(shapiro.test( as.numeric( df$Total ) )) # Use the p-value returned by the normality test


#ggplot of CO2 Emissions 1880-2010
yearsAdjusted1 <- subset(df, df$Year >= 1880)
windows()
ggplot( yearsAdjusted1, aes(x = Year,y = Total) ) + geom_line( color = "blue", linetype = "solid",  size = 1)

#ggplot of Sea Level 1880-2010
windows()
ggplot( yearsAdjusted2, aes(x = Year,y = CSIRO.Adjusted.Sea.Level, group = 1) ) + geom_line( color = "red", linetype = "solid",  size = 1)



#Density, QQ Plot, and Shapiro Test of CO2
windows()
plot(density(unlist(yearsAdjusted1[2])), main = "Density Plot of Total Global CO2 Emmisions 1880-2010")
windows()
qqnorm(unlist(yearsAdjusted1[2]), main = "QQ plot of Total Global CO2 Emmisions 1880-2010")
qqline(unlist(yearsAdjusted1[2]))
print(shapiro.test( as.numeric( yearsAdjusted1$Total ) )) # Use the p-value returned by the normality test

#Density, QQ Plot, and Shapiro Test of Sea Level
windows()
plot(density(unlist(yearsAdjusted2[2])), main = "Density plot of Cumulative Sea Levels 1880-2010")
windows()
qqnorm(unlist(yearsAdjusted2[2]), main = "QQ plot of Cumulative Sea Level Change 1880-2010")
qqline(unlist(yearsAdjusted2[2]))
print(shapiro.test( as.numeric( yearsAdjusted2$CSIRO.Adjusted.Sea.Level ) )) # Use the p-value returned by the normality test


#Adjustment of Sea Level to Make Magnitudes Similar
adjustedSeaLevel <- yearsAdjusted2[2]*500


#Hist of CO2
# Histogram
windows()
hist(unlist(yearsAdjusted1[2]), main="Global CO2 Emissions", xlab = 'Million Metric Tons of Carbon')

#Hist of Sea Level
windows()
hist(unlist(yearsAdjusted2[2]), main="Cumulative Global Sea Level Change", xlab = 'Inches')

# Dot plot of CO2 Emissions vs Sea Level Change (adjusted) dividing lines
windows()
plot(unlist(yearsAdjusted1[2]),unlist(adjustedSeaLevel[]), xlab = 'Sea Level * 500 to adjust magnitude', ylab = 'Global Co2 Emissions')
#abline(h=60,col = "red", lty = "dashed", lwd=2)
#abline(v=60,col = "red", lty = "dashed", lwd=2)
#abline(h=90,col = "springgreen4", lty = "longdash", lwd=2)
#abline(v=90,col = "springgreen4", lty = "longdash", lwd=2)

# Boxplot of CO2 Emissions
windows()
boxplot(unlist(yearsAdjusted1[2]),
        main = "Box Plot of Global CO2 Emissions per year", ylab = "Million Metric Tons of Carbon", 
        names=c("CO2 Emissions"))

# Boxplot of Sea Level Changes
windows()
boxplot(unlist(yearsAdjusted2[2]),
        main = "Box Plot of Cumulative Global Sea Level Changes", ylab = "Inches", 
        names=c("Sea Level Changes"))


# Plot the difference between Sea level Change and CO2 Emissions Unadjusted
windows()
plot(sort(unlist(yearsAdjusted1[2])-unlist(yearsAdjusted2[2])), main="Difference Between CO2 Emissions and Sea Level Change (unadjusted)", xlab="Year - 1880", ylab="Global CO2 Emissions  - Cumulative Global Sea Level Changes (unadjusted)" )
abline(h=0, col = "springgreen4", lwd=2)
# Plot the difference between Sea level Change and CO2 Emissions Adjusted
windows()
plot(sort(unlist(yearsAdjusted1[2])-unlist(adjustedSeaLevel[])), main="Difference Between CO2 Emissions and Sea Level Changes (adjusted)", xlab="Year - 1880", ylab="Global CO2 Emissions  - (Cumulative Global Sea Level Changes * 500)")
abline(h=0, col = "springgreen4", lwd=2)


#Calculation of delta CO2 and Sea Level
deltaCO2 <- yearsAdjusted1
deltaCO2[2] <- deltaCO2[2]-colMeans(deltaCO2[2])[col(deltaCO2[2])]


deltaSeaLevel <- yearsAdjusted2
deltaSeaLevel[2] <- deltaSeaLevel[2]-colMeans(deltaSeaLevel[2])[col(deltaSeaLevel[2])]


#ggplot of Delta CO2 Emissions 1880-2010
windows()
ggplot( deltaCO2, aes(x = Year, y = Total) ) + geom_line( color = "blue", linetype = "solid",  size = 1)

#ggplot of Delta Sea Level 1880-2010
windows()
ggplot( deltaSeaLevel, aes(x = Year, y = CSIRO.Adjusted.Sea.Level, group = 1) ) + geom_line( color = "red", linetype = "solid",  size = 1)



#Density, QQ Plot, and Shapiro Test of deltaCO2
windows()
plot(density(unlist(deltaCO2[2])), main = "Density Plot of Total delta Global CO2 Emmisions 1880-2010")
windows()
qqnorm(unlist(deltaCO2[2]), main = "QQ plot of Total delta Global CO2 Emmisions 1880-2010")
qqline(unlist(deltaCO2[2]))
print(shapiro.test( as.numeric( deltaCO2$Total ) )) # Use the p-value returned by the normality test



#Density, QQ Plot, and Shapiro Test of delta Sea Level
windows()
plot(density(unlist(deltaSeaLevel[2])), main = "Density plot of delta Cumulative Sea Levels 1880-2010")
windows()
qqnorm(unlist(deltaSeaLevel[2]), main = "QQ plot of delta Cumulative Sea Level Change 1880-2010")
qqline(unlist(deltaSeaLevel[2]))
print(shapiro.test( as.numeric( deltaSeaLevel$CSIRO.Adjusted.Sea.Level ) )) # Use the p-value returned by the normality test

#Hist of deltaCO2
windows()
hist(unlist(deltaCO2[2]), main="delta Global CO2 Emissions", xlab = 'Million Metric Tons of Carbon')

#Hist of delta Sea Level
windows()
hist(unlist(deltaSeaLevel[2]), main="delta Cumulative Global Sea Level Change", xlab = 'Inches')

# Boxplot of delta CO2 Emissions
windows()
boxplot(unlist(deltaCO2[2]),
        main = "Box Plot of delta Global CO2 Emissions per year", ylab = "Million Metric Tons of Carbon", 
        names=c("CO2 Emissions"))

# Boxplot of delta Sea Level Changes
windows()
boxplot(unlist(deltaSeaLevel[2]),
        main = "Box Plot of delta Cumulative Global Sea Level Changes", ylab = "Inches", 
        names=c("Sea Level Changes"))


#%%%%% NORMALITY VERIFICATION %%%%%
#CO2 Example
sampleVectorCO2[] <- yearsAdjusted1[2]

#take 1000 random samples means of sizes, 5, 30, 100
n = 1000
sampleMeans5CO2 <- c()
sampleMeans30CO2 <- c()
sampleMeans100CO2 <- c()
for (i in 1:n){
  sampleMeans5CO2[i] <-colMeans( sample_n(sampleVectorCO2, size = 5, replace = TRUE))
}
for (i in 1:n){
  sampleMeans30CO2[i] <-colMeans( sample_n(sampleVectorCO2, size = 30, replace = TRUE))
}
for (i in 1:n){
  sampleMeans100CO2[i] <-colMeans( sample_n(sampleVectorCO2, size = 100, replace = TRUE))
}


#Histogram to show distribution

windows()
hist(unlist(sampleMeans5CO2), col ='blue', xlab='Sample Mean of CO2 Emissions', main='Sample size = 5')
windows()
hist(unlist(sampleMeans30CO2), col ='blue', xlab='Sample Mean of CO2 Emissions', main='Sample size = 30')
windows()
hist(unlist(sampleMeans100CO2), col ='blue', xlab='Sample Mean of CO2 Emissions', main='Sample size = 100')

print(shapiro.test( as.numeric( sampleMeans5CO2 ) )) # Use the p-value returned by the normality test
print(shapiro.test( as.numeric( sampleMeans30CO2 ) )) # Use the p-value returned by the normality test
print(shapiro.test( as.numeric( sampleMeans100CO2 ) )) # Use the p-value returned by the normality test


#Sea Level  Example
sampleVectorSea <- yearsAdjusted2[2]

#take 1000 random samples means of sizes, 5, 30, 100
n = 1000
sampleMeans5Sea <- c()
sampleMeans30Sea <- c()
sampleMeans100Sea <- c()
for (i in 1:n){
  sampleMeans5Sea[i] <-colMeans( sample_n(sampleVectorSea, size = 5, replace = TRUE))
}
for (i in 1:n){
  sampleMeans30Sea[i] <-colMeans( sample_n(sampleVectorSea, size = 30, replace = TRUE))
}
for (i in 1:n){
  sampleMeans100Sea[i] <-colMeans( sample_n(sampleVectorSea, size = 100, replace = TRUE))
}




#Histogram to show distribution

windows()
hist(unlist(sampleMeans5Sea), col ='red', xlab='Sample Mean of Cumulative Sea Levels', main='Sample size = 5')
windows()
hist(unlist(sampleMeans30Sea), col ='red', xlab='Sample Mean of Cumulative Sea Levels', main='Sample size = 30')
windows()
hist(unlist(sampleMeans100Sea), col ='red', xlab='Sample Mean of Cumulative Sea Levels', main='Sample size = 100')

print(shapiro.test( as.numeric( sampleMeans5Sea ) )) # Use the p-value returned by the normality test
print(shapiro.test( as.numeric( sampleMeans30Sea ) )) # Use the p-value returned by the normality test
print(shapiro.test( as.numeric( sampleMeans100Sea ) )) # Use the p-value returned by the normality test


#Correlation testing
cor.test(unlist(yearsAdjusted1[2]), unlist(yearsAdjusted2[2]), method=c("pearson", "kendall", "spearman"))
cor.test(unlist(yearsAdjusted1[2]), unlist(yearsAdjusted2[2]), method = "spearman", exact = FALSE)





