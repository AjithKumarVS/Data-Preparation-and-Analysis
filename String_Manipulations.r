#Vector Definition
names <- c("Kermit Chacko",	
           "Eleonore Chien",
           "Genny Layne",
           "Willene Chausse",
           "Taylor Lyttle",
           "Tillie Vowell",
           "Carlyn Tisdale",
           "Antione Roddy",
           "Zula Lapp",
           "Delphia Strandberg",
           "Barry Brake",
           "Warren Hitchings",
           "Krista Alto",
           "Stephani Kempf",
           "Sebastian Esper",
           "Mariela Hibner",
           "Torrie Kyler")


#Prints the ninth Last Name
ninthName <-strsplit(names[9]," ")    
splitNinthName<-strsplit(unlist(ninthName)," ")
ninthLastName<-unlist(splitNinthName[2])
print(ninthLastName)


#Prints the number of Last Names starting with L
lastName <- strsplit(names," ")       
counter<-0
for (val in lastName){
  lName<-strsplit(unlist(val)," ")
  if (startsWith(unlist(lName[2]), "L")) {
    counter<- counter+1
  }
}
countLastNameStartsWithL<-counter
print(countLastNameStartsWithL)


#User Definied function which takes firstname and returns last name 
nameMap<-function(nam){                 
  namesplit <- strsplit(names," ")
  first<-c()
  last<-c()
  for (val1 in namesplit){
    name<-strsplit(unlist(val1)," ")
    first<-c(first,unlist(name[1]))
    last<-c(last,unlist(name[2]))
  }
  stop<-FALSE
  num<-0
  c<-0
  for(val2 in first){
    c<-c+1
    if (val2 == nam){
      num<-c
      stop<-TRUE
      break
    }
    if(stop){break}
  }
  names(first)<-last
  return(names(first[num]))
}
nameMap('Carlyn')
