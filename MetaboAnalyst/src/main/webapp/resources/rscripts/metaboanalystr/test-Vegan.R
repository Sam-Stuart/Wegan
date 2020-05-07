install.packages("vegan")
#install.packages("ggplot2")
library("vegan")
library("ggplot2")


NMDSWegan <- function(input,path = NULL,ext){
  Cairo::CairoFonts(regular="Arial:style=Regular",bold="Arial:style=Bold",italic="Arial:style=Italic",bolditalic = "Arial:style=Bold Italic",symbol = "Symbol")

  pathTest <- paste(getwd(),"WeganNMDS.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    nmdsPlot = metaMDS(dune)

    transferToGGplot(nmdsPlot,"WeganNMDS.png")

    #plot(nmdsPlot)
    
  }
  else{
    inputData = loadData(input, ext)
    nmdsPlot = metaMDS(inputData)

    transferToGGplot(nmdsPlot,"WeganNMDS.png")

    plot(nmdsPlot)
    
  }
  dev.off()
  #return()
}


CAWegan <- function(input,path = NULL,ext){
  
  pathTest <- paste(getwd(),"WeganCA.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    ca = cca(dune)
    plot(ca)
    
  }
  else{
    
    inputData = loadData(input, ext)
    ca = cca(inputData)
    plot(ca)
    
  }
  dev.off()
}


DCAWegan <- function(input,path = NULL,ext){

  pathTest <- paste(getwd(),"WeganDCA.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    dca = decorana(dune)
    plot(dca)
    
  }
  else{
    inputData = loadData(input, ext)
    dca = decorana(inputData)
    plot(dca)
    
  }
  dev.off()
  
}

#Simple function to allow proper loading function to be used based on
#What data format was uploaded to Wegan (Only csv/txt allowed)

loadData <- function(data,type){

    if(type == ".txt"){
        inputData = read.table(data)
    }else{
        inputData = read.csv2(data,row.names=1)
    }
    
    return (inputData)
}




transferToGGplot <- function(plot,name){

    data.scores = as.data.frame(scores(plot))
    nmdsplot = ggplot(data.scores,aes(x=NMDS1,y=NMDS2)) +
           geom_point(size=2) +
           theme_bw()+
           labs(title = "NMDS Plot")
    

    ggsave(name,nmdsplot,device = png())
    return("NMDS")

}