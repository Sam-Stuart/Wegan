#'Perform functional diversity
#'@description Perform functional diversity 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param corr Set correction method when the species-by-species distance matrix cannot be represented in a Euclidean space. Options are "sqrt", "cailliez" (default), "lingoes", or "none
#'@param w.abun Boolean weight FDis, Rao's Q, FEve, FDiv, and CWM by the relative abundances of the species, options are "TRUE" or "FALSE" (default)
#'@param stand.x Boolean if all traits are numeric, standardized to mean 0 and unit variance; if not, automatically use Gower's standardization by the range, options are "TRUE" of "FALSE" (default)
#'@param m the number of PCoA axes to keep as ‘traits’ for calculating FRic and FDiv, options are "min" (default), "max" or integer (over 1)
#'@param stand.FRic Boolean standardize FRic by the ‘global’ FRic that include all species, options are TRUE or FALSE(default)
#'@param print.pco Boolean return the eigenvalues and PCoA axes, options are TRUE or FALSE(default)
#'@param messages Boolean print warning messages in the console, options are TRUE or FALSE(default)
#'@param asym.bin set vector listing the asymmetric binary variables in data
#'@param ord Set the method for ordinal variables, options are "podani", "metric", "classic"
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export

#w = "",   
functionalDiv <- function(mSetObj = NA, data = "false", w_text = "", corr = "NULL", w.abun = "false", stand.x = "false",
                          m = "", stand.FRic = "false",
                          print.pco = "false", messages = "false", asym.bin = "", ord = "NULL") {
  #options(errors = traceback)

  #library("ade4")
  #library("adegraphics")
  #library("plyr")
  #library("dplyr")
  library("vegan")
  library("FD")
  
  mSetObj <- .get.mSet(mSetObj)

  data(tussock)
  print(tussock)
  metaData = tussock$trait
  mSetObj$dataSet$orig = tussock$abun 
  print(metaData)

  mSetObj$dataSet$norm <- tussock$abun
  mSetObj$dataSet$origMeta <- tussock$trait

  #Extract input from mSetObj
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  print(input)

  metaData <- mSetObj$dataSet$origMeta
  print(metaData)
  #w
  #print(w)

  #cat("Species data has to be numeric")
  #input.2 <- (input)
  
  if (corr == "NULL") { 
    corr1 = "cailliez"
  } else {
    corr1 <- corr
  }
  print(corr1)  

  if (w.abun == "false") { 
    w.abun1 = FALSE
  } else {
    w.abun1 = TRUE
  }
  print(w.abun1)
  
  if (stand.x == "false") { 
    stand.x1 = FALSE
  } else {
    stand.x1 = TRUE
  }
  print(stand.x1)
    
  #if (calc.FDiv == "false") { 
  #  calc.FDiv1 = FALSE
  #} else {
  #  calc.FDiv1 = TRUE
  #}
  #print(calc.FDiv1)
  
  #if (calc.FRic == "false") { 
  #  calc.FRic1 = FALSE
  #} else {
  #  calc.FRic1 = TRUE
  #}
  #print(calc.FRic1)
  
  if (stand.FRic == "false") { 
    stand.FRic1 = FALSE
  } else {
    stand.FRic1 = TRUE
  }
  print(stand.FRic1)
  
  #if (calc.CWM == "false") { 
  #  calc.CWM1 = FALSE
  #} else {
  #  calc.CWM1 = TRUE
  #}
  #print(calc.CWM1)
  
  if (print.pco == "false") { 
    print.pco1 = FALSE
  } else {
    print.pco1 = TRUE
  }
  print(print.pco1)
  
  if (messages == "false") { 
    messages1 = FALSE
  } else {
    messages1 = TRUE
  }
  print(messages1)
  
  if (m == "") {
    m1 = "max"
  } else if (m == "min") {
    m1 = "min"
  } else {
    m1 <- as.numeric(m)
  }
  print(m1)
  
  if (w_text == "") { #Nothing typed in text box
     ex10 <- dbFD(metaData, input, corr = corr1, w.abun = w.abun1, stand.x = stand.x1, 
               m = m1, stand.FRic = stand.FRic1, 
               print.pco = print.pco1, messages = messages1)     
  } else {
     print(w_text)
     w_text1 <- w_text #taken from text box by java, fed as string into R code
     #w_text1 <- gsub("\n", "", w_text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
     #w_text1 <- gsub(",", "+", w_text1, fixed=TRUE) 
     #w_text1 <- gsub(";", "+", w_text1, fixed=TRUE)
     #w_text1 <- gsub(":", "+", w_text1, fixed=TRUE)
     #w_text1 <- gsub("*", "+", w_text1, fixed=TRUE)
     #cat(paste0("You have selected these constraining variables: ", gsub("+", ", ", w_text1, fixed=TRUE), "."))
     #cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
     print(w_text1)
     w1 <- as.numeric(w_text1)
     print(w1)
     ex10 <- dbFD(metaData, input, w1, corr = corr1, w.abun = w.abun1, stand.x = stand.x1, 
               m = m1, stand.FRic = stand.FRic1, 
               print.pco = print.pco1, messages = messages1)
  }
    
  #ex10 <- dbFD(metaData, input, corr = corr1, w.abun = w.abun1, stand.x = stand.x1, 
  #             m = m1, stand.FRic = stand.FRic1, 
  #             print.pco = print.pco1, messages = messages1)
  print(ex10)  

  FRic <- as.data.frame(ex10$FRic)
  #colnames(FRic) <- c ("functional_richness")
  print("FRic")
  FEve <- as.data.frame(ex10$FEve)
  colnames(FEve) <- c ("functional_evenness")
  print("FEve")
  FDiv <- as.data.frame(ex10$FDiv)
  colnames(FDiv) <- c ("functional_divergence")
  print("FDiv")
  FDis <- as.data.frame(ex10$FDis)
  colnames(FDis) <- c ("functional_dispersion")
  print("FDis")
  RaoQ <- as.data.frame(ex10$RaoQ)
  colnames(RaoQ) <- c ("Rao's_quadratic_entropy")
  print("RaoQ")
  CWM <- as.data.frame(ex10$CWM)
  print("CWM")
  print(asym.bin)

  if(w_text == "") {
    if (asym.bin == "") {
        if (ord == "NULL") {
        exg <- gowdis(metaData)
        } else {
        ord1 <- ord
        exg <- gowdis(metaData, ord = ord1)
        }
    } else {
        asym.bin1 <- as.numeric(asym.bin)
        print(asym.bin1)
        if (ord == "NULL") {
          exg <- gowdis(metaData, asym.bin = asym.bin1)
        } else {
          ord1 <- ord
          exg <- gowdis(metaData, asym.bin = asym.bin1, ord = ord1)
        }
    }
} else {
    print(w_text1)  
    if (asym.bin == "") {
        if (ord == "NULL") {
           exg <- gowdis(metaData, w1)
        } else {
          ord1 <- ord
          exg <- gowdis(metaData, w1, ord = ord1)
        }
    } else {
        asym.bin1 <- as.numeric(asym.bin)
        print(asym.bin1)
        if (ord == "NULL") {
          exg <- gowdis(metaData, w1, asym.bin = asym.bin1)
        } else {
          ord1 <- ord
          exg <- gowdis(metaData, w1, asym.bin = asym.bin1, ord = ord1)
        }
    }
}
  #print(asym.bin1)
  #print(ord1)
  #print(exg)
  

  exg1 <- as.matrix(exg)
  
  mSetObj$analset$FRic <- FRic
  mSetObj$analset$FEve <- FEve
  mSetObj$analset$FDiv <- FDiv
  mSetObj$analset$FDis <- FDis
  mSetObj$analset$RaoQ <- RaoQ
  mSetObj$analset$CWM <- CWM
  mSetObj$analset$exg <- exg
  
  write.csv(FRic, "Functional richness.csv")
  write.csv(FEve, "Functional evenness.csv")
  write.csv(FDiv, "Functional divergence.csv")
  write.csv(FDis, "Functional dispersion.csv")  
  write.csv(RaoQ, "Rao's quadratic entropy.csv")  
  write.csv(CWM, "Community-level weighted means of trait values.csv")
  write.csv(exg1, "Gower dissimilarity.csv")

  return(.set.mSet(mSetObj))
}
 

#'Produce a cluster dendrogram based on Gower’s distancematrix
#'@description Produce a cluster dendrogram based on Gower’s distancematrix
#'@param mSetObj Input name of the created mSet Object
#'@param color options include "gray" & "black"(default), "skyblue" & "blue", "hotpink" & "red"
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
FD_cluster_plot <- function(mSetObj=NA, color="NULL", imgName, format="png", dpi=72, width=NA) {
  
  #library(vegan)
  #library(FD)  
  #library(ggplot2)
  #library(ggdendro) not availble for this version of R
  #library(Cairo)
  
  mSetObj <- .get.mSet(mSetObj)
  
  plot_data <- mSetObj$analse$exg
  #print(plot_data)  

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.Rarefaction <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  #abline(0, 1)
  
  if(color=="NULL") { 
    color1 <- c("gray", "black") 
  } else if (color== "blue") { 
    color1 <- c("skyblue", "blue")
  } else if (color == "red") { 
    color1 <- c("hotpink", "red") 
  }
  print(color1)  

  pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
  
  hcd <- as.dendrogram(hclust(plot_data))
  print(hcd)
  nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
  
  #windows(width = 20, height = 20)
  #plot(hclust(plot_data), hang = -1)
  
  #windows(width = 20, height = 20)
  plot(hcd, hang = -1, nodePar = nodePar, ylab = "Height", edgePar = list(col = color1, lwd = 2:1))
  title("Cluster dendrogram based on Gower’s distancematrix")
  
  dev.off()
  return(.set.mSet(mSetObj))
}





