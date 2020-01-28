#Vector Definition
myVector <- 99:20000; myVector  	

#Computing the sum of all vector elemets (Using R Function)
sum(myVector)       

#Computing the sum of all vector elements (Using User Defined Function)
mySumFunc <- function(myVector){	 
  sumvec <- 0
  for (val in myVector){
    sumvec<-sumvec +val
  }
  print(sumvec)
}
mySumFunc(myVector)


#Finding the minimum of all vector elements (Using R Function)
min(myVector)        

#Finding the minimum of all vector elements (Using User Defined Function)
myMinFunc <- function(myVector){	
  min1 <- myVector[1]
  for (val in myVector){
    if (min1 > val){
      min1 <- val
    }
  }
  print(min1)
}
myMinFunc(myVector)


#Finding the maximum of all vector elements (Using R Function)
max(myVector)       

#Finding the maximum of all vector elements (Using User Defined Function)
myMaxFunc <-function(myVector){      
  max1 <- myVector[1]
  for (val in myVector){
    if (max1 < val){
      max1 <- val
    }
  }
  print(max1)
}
myMaxFunc(myVector)                      


#Computing the median (Using R Function)
median(myVector)       

#Computing the median (Using User Defined Function)
myMedianFunc <-function(myVector){	
  value<-length(myVector)
  if(value%%2==0){
    med<-((myVector[value/2]+myVector[(value/2)+1])/2)
  }
  else{
    med<-myVector[(value/2)+1]
  }
  print(med)
}
myMedianFunc(myVector)


#User Definied Function which returns TRUE if the number is divisible by 127
divis <- function(num){        	
  if(num%%127==0){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}
isdivisible<-divis(254)
print(isdivisible)


#User Defined Function to count the number of integers in myVector that are divisible by 127
count<-function(myVector){
  c<-0
  for (val in myVector){
    if(val%%127==0){
      c<-c+1
    }
  }
  return (c)
}
countdivis<-count(myVector)
print(countdivis)