#Counts how many times each parameter z of letters occur sequentially in a string 
concate<-c()
str1<-c()
output<-c()
charCombos<-function(str,z){
  for (i in 1:(nchar(str)-z+1)){
    sp<-substr(str,i,(i+z-1))
    concate<-c(concate,sp)
  }
  old<-strsplit(concate," ")
  new<-old
  for(i in 1:(length(old))){
    cc<-0
    for(j in (i):(length(old))){
      if((is.null(unlist(new[i])))||(is.null(unlist(old[j])))){
        break
      }
      if(unlist(new[i]) == unlist(old[j])){
        cc<-cc+1
        if(i!=j){
          old[j] <- NULL
        }
      }
    }
    if(cc!=0){
      str1<-c(unlist(new[i]),cc)
      output<-c(output,str1)
    }
  }
  return(output)
}
charCombos("helloworld",3)


#Prints the ratio of words specifically containing q that are immediately followed by a u divided by total number of words containing q, using the previous function and a link to a dictionary of english words provided below

bigListOfWords <- readLines('https://raw.githubusercontent.com/dwyl/english-words/master/words.txt')

e<-0
f<-0
g<-0
count<-0
for (val in bigListOfWords){
  str1<-paste(charCombos(val,2),collapse = " ")
  
  if(grepl('qu',str1,ignore.case = TRUE)){
    e<-e+1
  }
  if(grepl('q',str1,ignore.case = TRUE)){
    f<-f+1
  }
  if(grepl("q-|q\\.|q\\'",str1,ignore.case = TRUE)){
    g<-g+1
  }
}
count_ratio<-(e/(f-g))
print(count_ratio)


#Prints the top 5 most commonly used letters after q that are NOT equal to u sorted in descending order of frequency.
qcount<-c()
alpha<-c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','v','w','x','y','z')
for(val in alpha){
  counter_alpha<-0
  for (vall in bigListOfWords){
    str1<-paste(charCombos(vall,2),collapse = " ")
    if(grepl(paste('q',val,sep = ""),str1,ignore.case = TRUE)){
      counter_alpha<-counter_alpha+1
    }
  }
  qcount<-c(qcount,counter_alpha)
  #print(paste('q',val,counter_alpha))
}
names(qcount)<-alpha

print(names(head(sort(qcount,decreasing = TRUE),5)))