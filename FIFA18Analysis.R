library(ggplot2)
library(dplyr)
library(gridExtra)

#first, we need to load our data

dataf = read.csv(file = "E:/data/fifa-18-demo-player-dataset/CompleteDataset.csv", stringsAsFactors = FALSE)
dataf = tbl_df(df)
dataf <- select(df, ID, X, Name, Age, Nationality, Overall, Club, Value, Wage, Preferred.Positions)

head(dataf, 10)

#some cleaning, we need to remove Euro symbol and turn "K" and "M" into numeric values
convertCurrency <- function(vector) {
  vector <- as.character(vector)
  vector <- gsub("(â‚¬|,)","", vector) #I had problem with reading "€" symbol in csv file, so I replaced what I was seeing instead
  result <- as.numeric(vector)
  
  kToThousands <- grep("K", vector)
  result[kToThousands] <- as.numeric(gsub("K","",        vector[kToThousands])) * 1000
  
  mToMillions <- grep("M", vector)
  result[mToMillions] <- as.numeric(gsub("M","", 
                                         vector[mToMillions])) * 1000000
  
  return(result)
}
dataf$Wage <- convertCurrency(dataf$Wage) 
dataf$Value <- convertCurrency(dataf$Value)
head(dataf,10)

#preparing to make a new column with more general position
dataf$Preferred.Positions <- gsub(" ", "", substr(dataf$Preferred.Positions, 1, 3))
head(dataf,10)

x<-as.factor(dataf$Preferred.Positions)

levels(x)<-list(GK=c("GK"),
                DEF=c("RB","RWB","CB","LB","LWB"),
                MID=c("CAM","CM","CDM"),
                WIN=c("LM","RM","LW","RW"),
                STR=c("ST","CF"))

dataf<-mutate(dataf,Position=x)
head(dataf,10)

#distribution of player's age
players_age<-ggplot(data=dataf,aes(Age))

players_age+
  geom_histogram(col="red",aes(fill=..count..))+
  ggtitle("PLayer's distribution based on age")
