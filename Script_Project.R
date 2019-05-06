library(ggplot2)
library(readr)
library(dplyr)
library(plyr)
library(sampling)
library(stringr)
library(rworldmap)
library(cowplot)

times <- read.csv("../University_Ranking/world-university-ranking/timesData.csv")
data <- read.csv("../University_Ranking/world-university-ranking/timesData.csv")

#Number of Universities in Top 100 for 2011
df <- data.frame(times[1:100,])
df <- droplevels(df)

p <- as.data.frame(table(df$country))
p

colnames(p) <- c("Country", "Frequency")

l <- ggplot(p, aes(x = Country, y = Frequency, fill = Country))+xlab("Country") +ylab("Number of Universities in Top 100")+geom_bar(stat = "identity")+ggtitle("Countrywise distribution of top 100 universities")+geom_text(aes(label = p$Frequency))
l + theme(
  text = element_text(size=12, colour = "Black"),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(size = 6,angle=90,vjust=1),
  axis.text.y = element_text(size = 8),
  legend.key = element_rect(fill = "white"),
  legend.position = "top",
  legend.text =  element_text(size = 8))+ylim(0, 60)

#Top 20 Universities in 2011
df1 <- data.frame(times[1:2603,])
df1 <- droplevels(df1)

p1 <- as.data.frame(table(df1$country))
colnames(p1) <- c("Country", "Frequency")

# Top 20 Universities
aa <- ggplot(df[1:20,], aes(x=world_rank, y=total_score, colour=country, label=university_name))+geom_text(check_overlap = TRUE, size=3, hjust = 0, nudge_x = 0.01, vjust = 1, nudge_y = 0.1, angle = 0)
aa + theme(
  axis.text = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  legend.position = "top",
  legend.text =  element_text(size = 8)
  )+xlab("World Rank")+ylab("Total Score")+scale_fill_hue(name="Country")

# All Universities
new_data<-filter(times,country=="United States of America")
bb <- ggplot(new_data, aes(x=world_rank, y=total_score, color=country))+geom_point()
bb+ theme(
  text = element_text(size=16, colour = "Navy"),
  axis.text = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  legend.key = element_rect(fill = "white"),
  legend.background = element_rect(fill = "#e0ffff"),
  legend.position = "top",
  legend.text =  element_text(size = 8),
  legend.direction = "horizontal",
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#FFCACB")
)

#Score of Top 3 Universities Harvard, MIT and Cal Tech

harvard <- data[grep("Harvard University",data$university_name),]

mit <- data[grep("Massachusetts Institute of Technology",data$university_name),]

caltech <- data[grep("California Institute of Technology",data$university_name),]

All <- rbind(harvard, caltech, mit)

g <- ggplot(All,aes(year,total_score,fill=university_name, colour=university_name, group= university_name)) + xlab("Year") + ylab("Score") + ggtitle("Overall Score of Top 3 Times Universities ") + geom_bar(stat = "identity", position="dodge")
g + theme(
  text = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  legend.key = element_rect(fill = "white"),
  legend.position = "top",
  legend.background = element_rect(color = "white"),
  legend.text = element_text(size = 12),
  panel.background = element_rect(fill = "white", size = 12)
  
)

#Ivy Leaague Ranking
Harvard <- data[grep("Harvard University",data$university_name),]
Harvard[,2] = 'Harvard'

Stanford <- data[grep("Stanford University",data$university_name),]
Stanford[,2] = 'Stanford'

Brown <- data[grep("Brown University",data$university_name),]
Brown[,2] = 'Brown'

#In grep command put 'Columbia University' instead of 'Columbia' because it will give you more universitites that are having Columbia in their name
Columbia <- data[grep("Columbia University",data$university_name),]
Columbia[,2] <- 'Columbia'

Cornell <- data[grep("Cornell",data$university_name),]
Cornell[,2] <- 'Cornell'

Dartmouth <- data[grep("Dartmouth",data$university_name),]
Dartmouth[,2] <- 'Dartmouth'

Princeton <- data[grep("Princeton",data$university_name),]
Princeton[,2] <- 'Princeton'

Yale <- data[grep("Yale",data$university_name),]
Yale[,2] <- 'Yale'

Penn <- data[grep("University of Pennsylvania",data$university_name),]
Penn[,2] <- 'Penn'

All <- rbind(Harvard,Stanford,Brown,Princeton,Columbia,Cornell,Dartmouth,Yale)

g <- ggplot(All,aes(year,world_rank,color=university_name,group=university_name)) + xlab("Year") + ylab("World Ranking") + ggtitle("Ivy League Ranking") + geom_point(size=2) + geom_line(size=1)
g + theme(
  text = element_text(size = 12, colour = "Black"),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  legend.key = element_rect(fill = "white"),
  legend.background = element_rect(fill="white"),
  legend.text = element_text(size = 12),
  legend.position = "top",
  panel.grid.minor = element_blank(),
  panel.background = element_rect(size = 12)
  
)   

#Research score in United States
suni<-subset(data,year==2011)
mm_2011<-ddply(suni,.(country),summarize,value=mean(research))

suni<-subset(data,year==2012)
mm_2012<-ddply(suni,.(country),summarize,value=mean(research))

suni<-subset(data,year==2013)
mm_2013<-ddply(suni,.(country),summarize,value=mean(research))

suni<-subset(data,year==2014)
mm_2014<-ddply(suni,.(country),summarize,value=mean(research))

suni<-subset(data,year==2015)
mm_2015<-ddply(suni,.(country),summarize,value=mean(research))

suni<-subset(data,year==2016)
mm_2016<-ddply(suni,.(country),summarize,value=mean(research))
Year<-c("2011","2012","2013","2014","2015","2016")
All<-rbind(mm_2011[26,c(1,2)],mm_2012[40,c(1,2)],mm_2013[41,c(1,2)],mm_2014[40,c(1,2)],mm_2015[41,c(1,2)],mm_2016[71,c(1,2)])
All<-cbind(All,Year)

ggplot(All,aes(y=value,x=Year,fill=Year))+geom_bar(stat = "identity", position="dodge")+xlab("Year")+ylab("Research Mean Score")+ggtitle("Research Score of United States between 2011-2016")+ylim(0,75)

#Teaching Score in 2015 and 2016
nyear<-c(2015,2016)

for(i in nyear) {
  
  suni<-subset(data,year==i)
  mm<-ddply(suni,.(country),summarize,value=mean(teaching))
  
  gtdMap <- joinCountryData2Map( mm, nameJoinColumn="country", joinCode="NAME" )
  mapCountryData( gtdMap, nameColumnToPlot='value', catMethod='fixedWidth', numCats=10,mapTitle=paste("Teaching in ",i))
}


#Research between UK and USA

suniIT<-subset(data,country=="United Kingdom")
suniUSA<-subset(data,country=="United States of America")

years<-unique(data$year)

pIT<-ggplot(suniIT,aes(x=research,color=factor(year)))+geom_density()+scale_x_continuous(limits = c(0, 100))+xlab("Research Score")+ylab("Density")+ggtitle("United Kingdom")+scale_color_discrete(name="Year");
pUSA<-ggplot(suniUSA,aes(x=research,color=factor(year)))+geom_density()+scale_x_continuous(limits = c(0, 100))+xlab("Research Score")+ylab("Density")+ggtitle("United States of America")+scale_color_discrete(name="Year");

plot_grid(pIT, pUSA)

#International Students in USA
ggplot(suniUSA,aes(y=international_students,x=factor(year),fill=factor(year)))+geom_bar(stat="identity")+xlab("Year")+scale_fill_discrete(name="Year")+ggtitle("International Students in United States of America")+theme(axis.text.x=element_text(angle = 90,vjust=1))+ylab("Percentage%")+ylim(c(0,20))

#Central Limit Theorem
layout(matrix(c(1,2,3,4),2,2, byrow=TRUE))

samples<-2603
sample.size<-100
xbar<-numeric(samples)
new_data<-filter(data,total_score!="-")
for(i in 1:samples)
{
  xbar[i]<-mean(sample(as.numeric(new_data$total_score),size=sample.size,replace=TRUE))
}
cat("mean=",mean(xbar),"sd=",sd(xbar))
hist(xbar,prob=TRUE,breaks=20,main="Sample Size=100",xlim=c(140,200))

samples<-2603
sample.size<-200
xbar<-numeric(samples)
new_data<-filter(data,total_score!="-")

for(i in 1:samples)
{
  xbar[i]<-mean(sample(as.numeric(new_data$total_score),size=sample.size,replace=TRUE))
}
cat("mean=",mean(xbar),"sd=",sd(xbar))

hist(xbar,prob=TRUE,breaks=20,main="Sample Size=200",xlim=c(140,200))

samples<-2603
sample.size<-300
xbar<-numeric(samples)
new_data<-filter(data,total_score!="-")

for(i in 1:samples)
{
  xbar[i]<-mean(sample(as.numeric(new_data$total_score),size=sample.size,replace=TRUE))
}

hist(xbar,prob=TRUE,breaks=20,main="Sample Size=300",xlim=c(140,200))
cat("mean=",mean(xbar),"sd=",sd(xbar))
samples<-2603
sample.size<-400
xbar<-numeric(samples)
new_data<-filter(data,total_score!="-")

for(i in 1:samples)
{
  xbar[i]<-mean(sample(as.numeric(new_data$total_score),size=sample.size,replace=TRUE))
}
cat("mean=",mean(xbar),"sd=",sd(xbar))
hist(xbar,prob=TRUE,breaks=20,main="Sample Size=400",xlim=c(140,200))

#Sampling
#Simple Sampling
s<-srswr(100,nrow(data))

sample.1<-data[s!=0,]
table(sample.1$country)

#Systematic Sampling
N<-nrow(data)
n<-100

k<-ceiling(N/n)
r<-sample(k,1)

s<-seq(r,by=k,length=n)
sample.2<-data[s,]
table(sample.2$country)

#Cluster Sampling
cl<-cluster(data,c("country"),size=2,method="srswor")
sample.3<-getdata(data,cl)
table(sample.3$country)

#Confidence Interval
conf<-c(80,90)
alpha<- 1-conf/100

sample.size<-100
sd.sample.means<-sd(as.numeric(new_data$total_score))/sqrt(sample.size)

sample.data<-sample(as.numeric(new_data$total_score), size=sample.size)
xbar<-mean(sample.data)

for(i in alpha)
{
  str<-sprintf("%2d%% Conf Level (alpha=%.2f), CI=%.2f-%.2f",100*(1-i),i,xbar-qnorm(1-i/2)*sd.sample.means,xbar+qnorm(1-i/2)*sd.sample.means)
  cat(str,"\n")
}
