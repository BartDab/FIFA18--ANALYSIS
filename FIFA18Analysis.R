library(ggplot2)
library(dplyr)
library(gridExtra)

#first, we need to load our data

dataf = read.csv(file = "E:/data/fifa-18-demo-player-dataset/CompleteDataset.csv", stringsAsFactors = FALSE)
dataf = tbl_df(dataf)
dataf <- select(dataf, ID, X, Name, Age, Nationality, Overall, Club, Value, Wage, Preferred.Positions)

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

#anlogically, let's made column with more general overall
y<-as.factor(dataf$Overall)

levels(y)<-list("46-50"=c(46,47,48,49),
                "50-59"=c(50,51,52,53,54,55,56,57,58,59),
                "60-69"=c(60,61,62,63,64,65,66,67,68,69),
                "70-79"=c(70,71,72,73,74,75,76,77,78,79),
                "80-89"=c(80,81,82,83,84,85,86,87,88,89),
                "90+"=c(90,91,92,93,94))

dataf<-mutate(dataf,Level=y)
head(dataf,10)

#distribution of player's age
players_age<-ggplot(data=dataf,aes(Age))
players_ovr<-ggplot(data=dataf,aes(Overall)) #hope this gonna be useful later

players_age+
  geom_histogram(col="red",aes(fill=..count..))+
  ggtitle("Players' distribution based on age")

players_age+
  geom_density(col="lightgreen", aes(fill = Position), alpha=0.8) + facet_grid(.~Position) + 
  ggtitle("Players' distribution based on age and position")



players_age+
  geom_density(col="lightgreen", aes(fill = Level), alpha=0.8) + facet_grid(.~Level) + 
  ggtitle("Players' distribution based on overall and position")


players_age+
  geom_density(col="lightgreen", aes(fill = Level), alpha=0.8) + 
  ggtitle("Players' distribution based on overall and position")

