#'Plotting Module R scripts 
#'@description Perform Plotting
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'@param mSetObj Input name of the created mSet Object
#'University of Alberta, Canada
#'License: 
#'@export
#'


plotLinearFunction <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA,
 type = "l", numlines = 1, colors = "black", weights = '1', pchs = '20', xlab = 'x axis',
 ylab = 'y axis', maintitle = 'Title', facA=NULL, facB=NULL){
    pchs = as.integer(pchs)
    mSetObj <- .get.mSet(mSetObj)


    #Dependent var default is first column. Independent var default is second column.


  #Set dependent (response) variable name
  if (facA == "NULL"){
    facA <- colnames(mSetObj$dataSet$norm)[1] #Default is first column.
  } else {
    facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
  }
  
  #Set independent (predictor) variable name
  if (facB == "NULL"){
    facB <- colnames(mSetObj$dataSet$norm)[2] #Default is second column.
  } else {
    facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
  }
   
  #Variable type check
  if (is.factor(mSetObj$dataSet$norm[,facA] || mSetObj$dataSet$norm[,facB])==TRUE){
    #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
  }
  
  #Define formula
  formula <- as.formula(paste0(facB, "~", facA)) 
  print(formula);

    print("BANANANANA");
    #data <- mSetObj$dataSet$orig
    data <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]

    print("facA: ");
   print(facA);
   print(data[facA]);
    print("facB: ");
   print(facB);
    print(data[facB]);
  
    
    #mSetObj <- list()
    #Set plot dimensions
    if(is.na(width)){
      w <- 7.2
    } else if(width == 0){
      w <- 7.2
    } else{
      w <- width
    }
    h <- w


    #Name plot for download
    imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
    mSetObj$imgSet$linear1 <- imgName
    
    #Generate plot
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    #print("plot linear function 2 " )
    plot <- plot(formula = formula, data = data, type = type, col = colors[1], lwd = weights[2], pch = pchs, xlab = xlab,ylab = ylab, main = maintitle )
    #plot(t,z);
    print(imgName);
    print("plot linear function 3 " );
    dev.off()

    #return(.set.mSet(mSetObj))
}  
   
lin.reg.columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  
  
  
  data <- select_if(mSetObj$dataSet$norm, is.numeric)
  count.all.numeric.cols <- ncol(data)
  name.all.numeric.cols <- colnames(data)
  
  num.col.results <- list(
    count=count.all.numeric.cols,
    names=name.all.numeric.cols
  )
  
  return(name.all.numeric.cols)
  
}

lin.reg.get.results <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj)
  lin.reg.result <- c(mSetObj$analSet$linReg1$res$equation, mSetObj$analSet$linReg1$res$r.squared.eq, mSetObj$analSet$linReg1$res$r.squared.adj.eq)
  return(lin.reg.result)

}

    