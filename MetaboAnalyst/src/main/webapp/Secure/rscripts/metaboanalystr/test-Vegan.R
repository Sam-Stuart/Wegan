


NMDSWegan <- function(input,ext,Meta = NULL, metaExt = NULL){

  pathTest <- paste(getwd(),"WeganNMDS.png",sep="/")

  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    nmdsPlot = metaMDS(dune)

    transferToGGplot(nmdsPlot,"NMDS")

    #plot(nmdsPlot)
    
  }
  else{
    inputData = loadData(input, ext)

    if(!is.null(Meta)){
        print("---------------------------") 
        print("---------------------------")

        metaData = loadData(Meta,metaExt)
        print(metaData)
        nmdsPlot = metaMDS(inputData)

        transferToGGplot(nmdsPlot,"NMDS",metaData)
    }
    else{
        nmdsPlot = metaMDS(inputData)

        transferToGGplot(nmdsPlot,"NMDS")
    }
    

    #plot(nmdsPlot)
    
  }
  dev.off()
  
}


CAWegan <- function(input,path = NULL,ext){
  
  pathTest <- paste(getwd(),"WeganCA.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    ca = cca(dune)
    
    transferToGGplot(ca,"CA")
    
  }
  else{
    
    inputData = loadData(input, ext)
    ca = cca(inputData)
    
    transferToGGplot(ca,"CA")
    
  }
  dev.off()
}


DCAWegan <- function(input=NULL,path = NULL,ext=NULL){

  pathTest <- paste(getwd(),"WeganDCA.png",sep="/")
  
  Cairo::Cairo(400,400,file=pathTest,type="png",bg="white")

  if(input == "Dune"){
    inputData = data(dune)
    dca = decorana(dune)
    transferToGGplot(dca,"DCA")
    #plot(dca)
  }
  else{
    inputData = loadData(input, ext)
    dca = decorana(inputData)
    transferToGGplot(dca,"DCA")
    #plot(dca)
  }
  dev.off()
  
}

testDCA <-function(){

    mSetObj <- .get.mSet(mSetObj)
    print(mSetObj$dataSet)


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




transferToGGplot <- function(plot,name,metaData = NULL){
    datascore <- scores(plot)
    data.scores = as.data.frame(scores(datascore))
    print(data.scores)
    
    if(name == "NMDS"){

        if(!is.null(metaData)){
            print("---------------------------") 
            print("---------------------------")
            
            nmdsData <- cbind(data.scores,metaData)
            print(nmdsData)
            rowName = names(nmdsData[3])
            print(rowName)
            plots = ggplot(nmdsData,aes(x=NMDS1,y=NMDS2, color = get(rowName))) +
               geom_point(size=3) +
               theme_bw()+
               labs(title = "NMDS Plot", color = rowName)

        }else{
        print("---------------------------") 
            print("Why tho?")
            plots = ggplot(data.scores,aes(x=NMDS1,y=NMDS2)) +
               geom_point(size=2) +
               theme_bw()+
               labs(title = "NMDS Plot")

        }
        
        
    }
    if(name == "CA"){
        plots = ggplot(data.scores,aes(x=CA1,y=CA2)) +
               geom_point(size=2) +
               theme_bw()+
               labs(title = "CA Plot")
    }

    if(name == "DCA"){
        plots = ggplot(data.scores,aes(x=DCA1,y=DCA2,color = 'red')) +
               geom_point(size=3) +
               theme_bw()+
               theme(legend.position = 'none')+
               labs(title = "DCA Plot")
    }
    
    
    filename <- paste("Wegan",name,".png",sep="")
    
    ggsave(filename,plots,device = png())
    return(name)

}





transferToJSON <- function(){
    
    mSetObj <- .get.mSet(mSetObj)
    print(mSetObj)

}

