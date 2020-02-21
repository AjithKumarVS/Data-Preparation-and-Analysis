#Importing Adult Data Set from UCI Machine Learning repository.
#Link:http://archive.ics.uci.edu/ml/datasets/Adult
adult <- read.csv("C:/Users/avakk/Downloads/adult.data", header=FALSE)


#Renaming the variables to be the proper names listed on the website
names(adult)<-c('age', 'workclass', 'fnlwgt', 'education', 'education-num', 'marital-status', 'occupation', 'relationship', 'race', 'sex', 'capital-gain', 'capital-loss', 'hours-per-week', 'native-country', 'incomeLevel')
View(adult)


#Labels all government employees as "government", all self-employeed employees as "selfEmployed", all Private employees as "Private" and everyone else as "Other".
workSector<-c()
column<-as.list(adult[2])
e<-0
for(va in unlist(column)){
  if(va == " Private"){
    workSector<-c(workSector,"Private")
  }
  else if((va == " Self-emp-not-inc") || (va == " Self-emp-inc")){
    workSector<-c(workSector,"selfEmployed")
  }
  else if((va == " Local-gov" ) || (va == " Federal-gov") || (va == " State-gov")){
    workSector<-c(workSector,"government")
    e<-e+1
  }
  else{
    workSector<-c(workSector,"Other")
  }
}
adult<-cbind(adult,workSector)
View(adult)

print("Number of Government Employees")
print(e)



#Creates a histogram of variable 'age'
hist(as.numeric((adult$age)))


#Prints the top 3 occupations with the highest average hours-per-week
tail(sort(tapply(adult$'hours-per-week', adult$occupation, mean)),3)



#Problem Statement: Your friend works for the government and claims that in order to make more money, you have to work for longer hours. Use this data set to determine if your friend is right and state your conclusion in the comments

#For initializing serial numbers to adult data set
myVec <- 1:length(adult$age)  
adult<-cbind(adult,myVec)

#Filters rows based on a condition
adult<-adult[(adult$workSector=="government"),]    
myVec <- 1:length(adult$age)
adult<-cbind(adult,myVec)

ggplot(adult,aes(x= adult$myVec, y = adult$`hours-per-week`))+geom_point(alpha = 0.1, aes(color = adult$incomeLevel))

#Based on the plot the conclusion he claims seems to be true except for minimal exceptions as seen in the plot. It is also evident that higher income is reflected for persons working longer hours
