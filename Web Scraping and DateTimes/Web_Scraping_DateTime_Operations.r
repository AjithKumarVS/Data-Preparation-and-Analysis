install.packages("lubridate")
library('lubridate')

library('rvest')
library('stringr')
library('tidyr')

#Converting HTML table into data frame

url = 'http://www.espn.com/nfl/superbowl/history/mvps'

webpage <- read_html(url)

sb_table <- html_nodes(webpage, css = 'table')
t <- sb_table[[1]]
sb <- html_table(sb_table)[[1]]
View(sb)
sb <- sb[-(1:2), ]
names(sb)<-c('NO','Player','Highlights')

#Extracting the names of the MVPs, Position and Team into columns MVP1, MVP2, Position, Team
sb<-separate(sb,Player,c('MVP','Position','Team'),sep = ',',remove = TRUE)
sb<-separate(sb,MVP,c('MVP1','MVP2'),sep='&',remove=TRUE)

head(sb)

# 90th%, 92.5th%, 95th%, 97.5th% and 99th% confidence intervals for the mean of passing yards
 
yards<-c()
for(i in 1:nrow(sb)){
  strings<-sb$Highlights[i]
  pos = regexpr('yards', strings)
  final<-substr(strings,pos-4,pos-2)
  if(!is.na(as.integer(final))){
    yards<-c(yards, as.integer(final))
  }
}
print(yards)
n<-length(yards)
m<-mean(yards)
s<-sd(yards)

error<-qt(0.95,n-1)*(s/sqrt(n))
h1<-m+c(-error,error)
print(h1)

error<-qt(0.9625,n-1)*(s/sqrt(n))
h2<-m+c(-error,error)
print(h2)

error<-qt(0.975,n-1)*(s/sqrt(n))
h3<-m+c(-error,error)
print(h3)

error<-qt(0.9875,n-1)*(s/sqrt(n))
h4<-m+c(-error,error)
print(h4)

error<-qt(0.995,n-1)*(s/sqrt(n))
h5<-m+c(-error,error)
print(h5)

#Performing ANOVA on following data

food1 <- c(164,   172,   168,   177, 	156, 	195)
food2 <- c(178,   191, 	 197, 	182, 	185, 	177)
food3 <- c(175,   193, 	 178, 	171, 	163, 	176)
food4 <- c(155,   166, 	 149, 	164, 	170, 	168)

meanfood1<-mean(food1)
meanfood2<-mean(food2)
meanfood3<-mean(food3)
meanfood4<-mean(food4)

allObs<-c(food1, food2, food3, food4)
meanallObs<-mean(allObs)

nfood1<-length(food1)
nfood2<-length(food2)
nfood3<-length(food3)
nfood4<-length(food4)
n<-length(allObs)

SSB<-(nfood1*(meanfood1-meanallObs)^2+nfood2*(meanfood2-meanallObs)^2+nfood3*(meanfood3-meanallObs)^2+nfood4*(meanfood4-meanallObs)^2)
SSE<-sum((food1-meanfood1)^2)+sum((food2-meanfood2)^2)+sum((food3-meanfood3)^2)+sum((food4-meanfood4)^2)

df1<- 4-1
df2<- n-4

MSB<-SSB/df1
MSE<-SSE/df2

F<-MSB/MSE
alpha<-0.05
critical_value <- qf(p = 1-alpha, df1 = df1, df2= df2)

ifelse(F>=critical_value, "Reject H0", "Fail to Reject Ho")

print(1-pf(F,df1 = df1,df2 = df2))

#Using Function
treatment <- c(
    rep('food1', 6)
  , rep('food2', 6)
  , rep('food3', 6)
  , rep('food4', 6)
)
weightLoss <- c(food1, food2, food3, food4)

df = data.frame(treatment, weightLoss)

fit <- aov(weightLoss ~ treatment)
summary(fit)


# Prints the number of Tuesdays which fell on the first of the month during the 19th century (1 Jan 1801 to 31 Dec 1901)
startDate <- dmy("01-Jan-1801")
endDate <- dmy("31-Dec-1901")
counter<-0
myDates <-seq(from = startDate, to = endDate, by = "days")
for(i in 1:length(myDates)){
  d<-str_split(myDates[i],"-")
  dd<-substr(d,18,19)
  if(as.integer(dd)==1 & wday(myDates[i], label = TRUE)=="Tue"){
    counter<-counter+1
  }
}
print(counter)


