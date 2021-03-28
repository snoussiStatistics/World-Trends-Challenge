#-----------------------------------

# Birth rate and internet usage by group income
P2.Demographic.Data <-
  read.csv(
    "C:/Users/Hamza/Desktop/BioStatistics/Udemy R/Exercices Dataset/P2-Demographic-Data.csv"
  )

P2.Demographic.Data -> stats

rm(P2.Demographic.Data)

library(ggplot2)

# Visualization

qplot(
  data = stats ,
  x = Birth.rate ,
  y = Internet.users ,
  size = I(4) ,
  colour = Income.Group,
  alpha = I(0.6) , 
  main = "Birth rate and internet usage by group income"
)


#-----------------------------------
# Birth rate and internet usage by region

source('C:/Users/Hamza/Desktop/BioStatistics/Udemy R/Exercices Dataset/CountryRegionVectors.R')

newdf <- data.frame(Codes_2012_Dataset ,
             Countries_2012_Dataset,
             Regions_2012_Dataset)

colnames(newdf) <- c("Codes" , "Names" , "Region")


stats <- merge(stats, newdf , by.x = "Country.Code" , by.y = "Codes")

rm(newdf)

stats$Names <- NULL

#head(stats)

rm(Codes_2012_Dataset, Countries_2012_Dataset)

# Visualization

qplot(
  data = stats ,
  x = Birth.rate ,
  y = Internet.users ,
  size = I(4) ,
  colour = Region ,
  alpha = I(0.6),
  main = "Birth rate and internet usage by region"
)
#-----------------------------------

# Birth rate mean and internet usage mean by region

Regions <- levels(factor(Regions_2012_Dataset))

meanByregion2013<- matrix(NA , nrow = length(Regions) , ncol = 2)

rownames(meanByregion2013
) <- Regions

colnames(meanByregion2013
) <- c("Birth_rate_mean", "Inernet_usage_mean")

for (i in Regions) {
  D = stats[stats$Region == i, ]
  
  meanByregion2013[i, 1] = mean(D$Birth.rate)
  meanByregion2013[i, 2] = mean(D$Internet.users)
  
}
meanByregion2013<- data.frame(meanByregion2013
)


# Visualization

qplot(
  data = meanByregion2013
,
  x = Birth_rate_mean ,
  y = Inernet_usage_mean,
  size = I(4) ,
  colour = Regions ,
  main = "Mean of Birth rate and internet usage by region"
)


rm(D, i, Regions_2012_Dataset)

#-----------------------------------

# fertility rate and Life expactency at birth 

newData <- read.csv("C:/Users/Hamza/Desktop/BioStatistics/Udemy R/Exercices Dataset/P2-Section5-Homework-Data.csv")

year1960 <- newData[newData$Year==1960,]

year2013 <- newData[newData$Year==2013,]

rm(newData)

source('C:/Users/Hamza/Desktop/BioStatistics/Udemy R/Exercices Dataset/Section5-Homework-Vectors.R')


LifeExp1960 <- data.frame(Life_Expectancy_At_Birth_1960 , Country_Code)

colnames(LifeExp1960) <- c("Life.Expectancy.At.Birth" , "Country_Code")

LifeExp2013 <- data.frame(Life_Expectancy_At_Birth_2013 , Country_Code)

colnames(LifeExp2013) <- c("Life.Expectancy.At.Birth" , "Country_Code")

rm(Life_Expectancy_At_Birth_1960,Life_Expectancy_At_Birth_2013,Country_Code)

year1960 <- merge(year1960 , LifeExp1960 , by.x = "Country.Code" , by.y = "Country_Code" )

year2013 <- merge(year2013 , LifeExp2013 , by.x = "Country.Code" , by.y = "Country_Code" )

year1960$Year <- NULL

year2013$Year <- NULL

rm(LifeExp1960,LifeExp2013)

# Visualization

qplot( data = year1960 ,
       x = Fertility.Rate ,
       y = Life.Expectancy.At.Birth , 
       color = Region , 
       size = I(4) , 
       alpha = I(0.6) , 
       main="Fertility rate and Life expactency by region 1960")

qplot( data = year2013 , 
       x = Fertility.Rate , 
       y = Life.Expectancy.At.Birth , 
       color = Region ,
       size = I(4) , 
       alpha = I(0.6), 
       main="Fertility rate and Life expactency by region 2013" )







#-----------------------------------

# Life expactency and Fertility rate mean by region

mean1960 <- matrix(NA , nrow = length(Regions)  , ncol = 2 )

rownames(mean1960) <- Regions
colnames(mean1960) <- c("Life_Exp_Mean1960" , "Fertility_Rate_Mean1960")

for ( i in Regions){
  
  D = year1960[year1960$Region== i,]
  
  mean1960[i,1] <- mean(D$Life.Expectancy.At.Birth)
  mean1960[i,2] <- mean(D$Fertility.Rate)
}


mean2013 <- matrix(NA , nrow = length(Regions)  , ncol = 2 )

rownames(mean2013) <- Regions
colnames(mean2013) <- c("Life_Exp_Mean2013" , "Fertility_Rate_Mean2013")

for ( i in Regions){
  
  D = year2013[year2013$Region== i,]
  
  mean2013[i,1] <- mean(D$Life.Expectancy.At.Birth)
  mean2013[i,2] <- mean(D$Fertility.Rate)
}

newMetrics <- cbind(mean1960,mean2013)

meanByRegion1960 <- data.frame(newMetrics)

rm(D,i,newMetrics,mean1960,mean2013)


# Visualisation 


qplot(data = meanByRegion1960 , 
      x = Life_Exp_Mean1960 , 
      y = Fertility_Rate_Mean1960 , 
      color = Regions,
      size = I(4) ,
      main = "Life expactency ~ Fertility rate mean by region 1960")


qplot(data = meanByRegion1960 , 
      x = Life_Exp_Mean2013 , 
      y = Fertility_Rate_Mean2013 , 
      color = Regions,
      size = I(4) ,
      main ="Life expactency ~ Fertility rate mean by region 2013")
#-----------------------------------------------------

IncomeGroupe <- data.frame(stats$Country.Code,stats$Income.Group,stringsAsFactors = T)
colnames(IncomeGroupe) <- c("Country.Code" ,"Income.Groupe")

year2013 <- merge(year2013 ,IncomeGroupe , by.x = "Country.Code" , by.y = "Country.Code" )





ggplot(data = year2013 , aes( x = Fertility.Rate , y = Life.Expectancy.At.Birth) ) +
  geom_point(aes(color=year2013$Income.Groupe))





































