install.packages("corrplot")
install.packages('mlbench')
library("corrplot")
library("glue")
library('mlbench')

#Loading Boston Housing data set
data(BostonHousing)
View(BostonHousing)

# Outputs the scatterplot matrix of all variables in the data set
pairs(BostonHousing)

# Programmatically generates box plot for each variable 

for (i in 1:ncol(BostonHousing)){
  if(is.numeric(BostonHousing[,i])){
    boxplot((BostonHousing[,i]),main=names(BostonHousing)[i],horizontal = TRUE)
  }
}

# Correlation matrix and correlation plot of data set

#Removing Factor variables
BostonHousing<-BostonHousing[,-4]

correlation<-cor(BostonHousing)
corrplot(cor(BostonHousing))
print(correlation)


# Identifying the top 3 strongest absolute correlations 
correlation<-cor(BostonHousing)
x<-0
y<-0
for (m in 1:3){
  max<-0
  for (i in 1:nrow(correlation)){
    for(j in 1:ncol(correlation)){
      if(max <= abs(correlation[i,][j]) & correlation[i,][j]!=1){
        max<-abs(correlation[i,][j])
        x<-i
        y<-j
      }
    }
  }
  correlation[x,][y]<-0
  correlation[y,][x]<-0
  a<-colnames(BostonHousing[y])
  b<-unname(max)
  c<-colnames(BostonHousing[x])
  top3<-glue("{a}-{c}:- {b}")
  print(top3)
}


# Creating a new variable "ageGroup" and assigning values based on different age quartiles
BostonHousing <- within(BostonHousing, ageGroup <- as.integer(cut(age, quantile(age, probs=0:4/4), include.lowest=TRUE)))
View(BostonHousing)
