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
  vector <- gsub("(â‚¬|,)","", vector)
  result <- as.numeric(vector)
  
  kToThousands <- grep("K", vector)
  result[kToThousands] <- as.numeric(gsub("K","", vector[kToThousands])) * 1000
  
  mToMillions <- grep("M", vector)
  result[mToMillions] <- as.numeric(gsub("M","", vector[mToMillions])) * 1000000
  
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

#making a column with more general overall

ovr_breaks <- c(0, 50, 60, 70, 80, 90, Inf)
ovr_labels <- c("50 or less", "50-60", "60-70", "70-80", "80-90", "90 or higher")
ovr <- cut(x=dataf$Overall, breaks=ovr_breaks, 
           labels=ovr_labels, include.lowest = TRUE)
dataf <- mutate(dataf, ovr)

head(dataf,20)

players_age<-ggplot(data=dataf,aes(Age))

#distribution of player's age
players_age+
  geom_histogram(col="red",aes(fill=..count..))+
  ggtitle("Players' distribution based on age")

#divided into positions
players_age+
  geom_density(col="lightgreen", aes(fill = Position), alpha=0.8) + facet_grid(.~Position) + 
  ggtitle("Players' distribution based on age and position")

#dependence between age and overall
players_age+
  geom_density(col="yellow", aes(fill = ovr), alpha=0.8) + 
  ggtitle("Dependence between age and overall")

players_ovr<-ggplot(data=dataf,aes(Overall))

#distribution basen on overall
players_ovr+
  geom_histogram(col="orange",aes(fill=..count..))+
  ggtitle("Players' distributuion basen od overall")

countries_count<-count(dataf,Nationality)
top20_countries<-top_n(countries_count,20,n)
top20_countries_names<-top20_countries$Nationality
country<-filter(dataf,Nationality==top20_countries_names)

#checking number of players from countries
ggplot(country,aes(x=Nationality))+
  geom_bar(col="greenyellow",aes(fill=..count..))+
  ggtitle("TOP 20 most numerous countries")

#highest 1% values
value_1<-quantile(dataf$Value,probs=0.99)
value_filter<-filter(dataf,Value>value_1)
value_result<-ggplot(value_filter,aes(Value))

value_result+geom_histogram(aes(fill=..count..))+
  ggtitle("Top 1% values' distrubution")

#highest 1% wages
wage_1<-quantile(dataf$Wage, probs=0.99)
wage_filter<-filter(dataf,Wage>wage_1)
wage_result<-ggplot(wage_filter,aes(Wage))

wage_result+
  geom_histogram(aes(fill=..count..))+
  ggtitle("Top 1% wages' distribution")

# Create wage brackets
w_breaks <- c(0, 100000, 200000, 300000, 400000, 500000, Inf)
w_labels <- c("0-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k or higher")
w_brackets <- cut(x=dataf$Wage, breaks=w_breaks, 
                     labels=w_labels, include.lowest = TRUE)
dataf <- mutate(dataf, w_brackets)
# Create value brackets

v_breaks <- c(0, 10000000, 20000000, 30000000, 40000000, 50000000, 60000000, 70000000, 80000000, 90000000, 100000000, Inf)
v_labels <- c("0-10M", "10-20M", "20-30M", "30-40M", "40-50M","50-60M", "60-70M", "70-80M", "80-90M","90-100M","100M or higher")
v_brackets <- cut(x=dataf$Value, breaks=v_breaks, 
                      labels=v_labels, include.lowest = TRUE)
dataf <-mutate(dataf, v_brackets)
head(dataf)

#big amount of players have wages lower than 100 000 or value lower than 30M, 
#so we won't include this to have more readable graph

graphWithout100K<-filter(dataf,w_brackets!="0-100k")

ggplot(graphWithout100K,aes(x=w_brackets))+
  geom_bar(aes(fill=..count..))+
  ggtitle("Distruibution of wages (100K+")

graphWithout30M<-filter(dataf,Value>30000000)

ggplot(graphWithout30M,aes(x=v_brackets))+
  geom_bar(aes(fill=..count..))+
  ggtitle("Distribution of values (30M+")

players_age_overall<-ggplot(dataf,aes(Age,Overall))

players_age_overall+geom_point(aes(color=v_brackets))+
  geom_smooth(color="brown")+
  ggtitle("Distribution between age and overall based on value")
  
players_age_overall+geom_point(aes(color=w_brackets))+
  geom_smooth(color="brown")+
  ggtitle("Distribution between age and overall based on value")

players_age_overall+geom_point(aes(color=ovr))+
  geom_smooth(color="brown")+
  ggtitle("Distribution between age and overall based on value")

ggplot(dataf, aes(Preferred.Positions))+
  geom_bar(aes(fill=..count..))+
  ggtitle("Position's distribuion")

#checking most valuable players by position

pvalue<-filter(dataf,Value<20000000)

pvalueplot<-ggplot(pvalue,aes(Preferred.Positions))+geom_bar(aes(fill=v_brackets))+ggtitle("Position based on Value")

pvalue2<-filter(dataf,Value>20000000)

pvalue2plot<-ggplot(pvalue2,aes(Preferred.Positions))+geom_bar(aes(fill=v_brackets))+ggtitle("Position based on Value part 2")

grid.arrange(pvalueplot,pvalue2plot,ncol=1)

#checking best paid players

pwage<-filter(dataf,Wage>100000,Wage<200000)

pwageplot<-ggplot(pwage,aes(Preferred.Positions))+geom_bar(aes(fill=w_brackets))+ggtitle("Position based on wage")

pwage2<-filter(dataf,Wage>200000)

pwage2plot<-ggplot(pwage2,aes(Preferred.Positions))+geom_bar(aes(fill=w_brackets))+ggtitle("Position based on wage part 2")

grid.arrange(pwageplot,pwage2plot,ncol=1)

#checking club's value
by_clubs <- group_by(dataf, Club)
club_v <- summarise(by_clubs, total_v = sum(Value))
top20_club_v <- top_n(club_v,20,total_v)
top20_club_v$Club<-as.factor(top20_club_v$Club)
ggplot(top20_club_v,aes(x=Club,y=total_v))+
  geom_bar(stat="identity",aes(fill=total_v))+coord_flip()+
  ggtitle("Top 20 valuable clubs")