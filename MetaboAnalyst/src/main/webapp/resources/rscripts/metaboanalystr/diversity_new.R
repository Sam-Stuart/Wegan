library(vegan)
library(ggplot2)
library(Cairo)


#Pielou's evenness function
PielouEvenessWegan <- function(){
  mSetObj <- .get.mSet(mSetObj);
  if(input == "BCI"){
    data(BCI)
    inputData <- BCI
    H <- diversity(inputData)
    J <- H/log(specnumber(inputData)) #where specnumber is a simple vegan function to find the numbers of species
    print(J)
  }
  
  else{
    #inputData <- loadData(input)

    H <- diversity(mSetObj$dataSet$norm)
    J <- H/log(specnumber(mSetObj$dataSet$norm))

    mSetObj$analSet$Eveness$h <- H
    mSetObj$analSet$Eveness$J <- J
    print(J)
  }
  return(.set.mSet(mSetObj));
  dev.off
}


#R�nyi diversity function
RenyiDiversityWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)
    inputData <- BCI
    k <- sample(nrow(inputData), 6)
    R <- renyi(inputData[k,])
    print(R)
  }
  
  else{
    inputData <- loadData(input)
    k <- sample(nrow(inputData), 6) #takes a sample of 6 observations
    R <- renyi(inputData[k,])
    print(R)
  }
  
  dev.off
}


#alpha parameter of Fisher's log-series diversity function
AlphaFisherWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    alpha <- fisher.alpha(inputData)
    print(alpha)
  }
  
  else{
    inputData <- loadData(input)
    alpha <- fisher.alpha(inputData)
    print(alpha)
  }
  
  dev.off
}


#Rarefied richness function for two individuals (finite sample variant of Simpson's index) 
RarefyRichnessWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    Srar <- rarefy(inputData, min(rowSums(inputData)))
    print(Srar)
  }
  
  else{
    inputData <- loadData(input)
    Srar <- rarefy(inputData, min(rowSums(inputData)))
    print(Srar)
  }
  
  dev.off
}


#Fisher model for species abundance function
FisherSpeciesAbundanceWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    k <- sample(nrow(inputData), 1)
    fish <- fisherfit(inputData[k,])
    print(fish)
  }
  
  else{
    inputData <- loadData(input)
    k <- sample(nrow(inputData), 1)
    fish <- fisherfit(inputData[k,])
    print(fish)
  }
  
  dev.off
}


#Preston model for species abundance function
PrestonSpeciesAbundanceWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    P <- prestondistr(inputData[k,])
    print(P)
  }
  
  else{
    inputData <- loadData(input)
    P <- prestondistr(inputData[k,])
    print(P)
  }
  
  dev.off
}


#Ranked abundance distribution model function 
RankedAbundWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    rad <- radfit(BCI[k,])
    print(rad)
  }
  
  else{
    inputData <- loadData(input)
    rad <- radfit(BCI[k,])
    print(rad)
  }
  
  dev.off
}


#Estimate unseen species function
EstUnseenSpeciesWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    E <- estimateR(inputData[k,])
    print(E)
  }
  
  else{
    inputData <- loadData(input)
    E <- estimateR(inputData[k,])
    print(E)
  }
  
  dev.off
}


#Extrapolating richness of species pool function
ExtrapRichnessWegan <- function(input){
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    S <- specpool(inputData)
    print(S)
  }
  
  else{
    inputData <- loadData(input)
    S <- specpool(inputData)
    print(S)
  }
  
  dev.off
}


#Species accumulation model function
SpeciesAccumWegan <- function(input){
  
  pathTest <- paste(getwd(),"WeganSpeciesAccumulation.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")
  
  if(input == "BCI"){
    data(BCI)     
    inputData <- BCI
    sac <- specaccum(inputData)
    plot(sac, ci.type="polygon", ci.col="lightblue", xlab="Number of individuals", ylab="Number of species")
  }
  
  else{
    inputData <- loadData(input)
    sac <- specaccum(inputData)
    plot(sac, ci.type="polygon", ci.col="lightblue", xlab="Number of individuals", ylab="Number of species")
  }
  
  dev.off
}
