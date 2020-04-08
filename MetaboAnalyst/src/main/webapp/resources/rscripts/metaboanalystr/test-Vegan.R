install.packages("vegan")
library("vegan")




NMDSWegan <- function(input,path = NULL){

  pathTest <- paste(getwd(),"WeganNMDS.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    nmdsPlot = metaMDS(dune)
    plot(nmdsPlot)
    
  }
  else{
    inputData = input
    nmdsPlot = metaMDS(input)
    plot(nmdsPlot)
    
  }
  dev.off()
}


CAWegan <- function(input,path = NULL){

  pathTest <- paste(getwd(),"WeganCA.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    ca = cca(dune)
    plot(ca)
    
  }
  else{
    inputData = input
    ca = cca(input)
    plot(ca)
    
  }
  dev.off()
}


DCAWegan <- function(input,path = NULL){

  pathTest <- paste(getwd(),"WeganDCA.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    dca = decorana(dune)
    plot(dca)
    
  }
  else{
    inputData = input
    dca = decorana(input)
    plot(dca)
    
  }
  dev.off()
}














getTempDir <- function(){
    return(getwd())
}