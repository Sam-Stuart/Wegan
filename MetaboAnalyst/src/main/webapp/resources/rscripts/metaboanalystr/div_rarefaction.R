#'Perform Rarefaction
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param type Set type for different types of rarefied community data, drop down options are "Species richness" (default), "Random rarefaction" or "Probability"
#'@param sample Input subsample size for rarefying community, either a single value or a vector.default is not apply subsample
#'@param se Input estimate standard errors, drop down options are "TRUE" or "FALSE" (default). 
#'@param margin Input the index is computed, drop down options are "1" (default) or "2" 
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export


Rarefaction_div <- function(mSetObj = NA, data = "false", type = "NULL", sample = "", se = "false", margin = "NULL") {
  
  options(errors = traceback)
  #print("inside faction R file")

  #library("ade4")
  #library("adegraphics")
  install.packages("plyr")
  library("plyr")
  #print("library plyr")
  library("dplyr")
  library("vegan")
  
  print("setup mSetObj")

  #Extract input from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  #input <- as.data.frame(input)
  
  print("margin setup")
  # margin = 1, data in rows; margin = 2, data in columns
  if (margin == "NULL") { 
    margin1 <- "1"
  } else if (margin == "2") {
    margin1 <- "2"  
  }
  margin1 <- as.numeric(margin1)

  cat("after if margin statmentes")
  #sample <- as.numeric(sample)
  if (sample == "") {
    #the minimum sample count achieved over the selected data columns
    sample1 <-  min(rowSums(input)) 
  } else {
    sample1 = as.numeric(sample)
  }

  print("before se")
  if (se == "false") {
    se1 = FALSE
  } else {
    se1 = TRUE
  }
 

  cat("Rarefaction technique works on numeric data only")
  input.2 <- select_if(input, is.numeric)


  print("before real analysis")
  if (type == "NULL") {
    type1 <- "Rarefaction"
    Srare <- rarefy(input.2, sample = sample1, se = se1, MARGIN = MARGIN1)
    cat ("Expected species richness in random subsamples of size 'samples' from the community")
    mSetObj$analset$result$name <- "Rarefaction"
    mSetObj$analset$result$type <- type1
    mSetObj$analset$result$sample <- sample1
    #mSetObj$analset$result$margin <- MARGIN1
    mSetObj$analset$result$Output <- Srare
  } else if (type == "Random_rarefaction") {
    Srare <- rrarefy(input.2, sample = sample1)
    cat("Generates one randomly rarefied community data frame or vector of given 'sample' size")
    mSetObj$analset$resdata$name <- "Rarefaction"
    mSetObj$analset$resdata$type <- type
    mSetObj$analset$resdata$sample <- sample1
    #mSetObj$analset$result$margin <- MARGIN1
    mSetObj$analset$resdata$Output <- Srare 
    Srare.frame <- as.data.frame(cbind(mSetObj$analset$resdata$name, mSetObj$analset$resdata$type, 
                                 mSetObj$analset$resdata$sample <- sample1, mSetObj$analset$resdata$Output <- Srare))
    mSetObj$analset$result <- Srare.frame 
  } else if (type == "Probability") {
    Srare <- drarefy(input.2, sample = sample1)
    cat("Returns probabilities that species occur in a rarefied community of size 'sample'") 
    mSetObj$analset$resdata$name <- "Rarefaction"
    mSetObj$analset$resdata$type <- type
    mSetObj$analset$resdata$sample <- sample1
    #mSetObj$analset$result$margin <- MARGIN1
    mSetObj$analset$resdata$Output <- Srare 
    Srare.frame <- as.data.frame(cbind(mSetObj$analset$resdata$name, mSetObj$analset$resdata$type, 
                                       mSetObj$analset$resdata$sample <- sample1, mSetObj$analset$resdata$Output <- Srare))
    mSetObj$analset$result <- Srare.frame 
    
  }

  print("transfer data")
  print(Srare)
  #mSetObj$analset$result$name <- "Rarefaction"
  #mSetObj$analset$result$type <- type
  #mSetObj$analset$result$sample <- sample1
  #mSetObj$analset$result$Output <- Srare
  mSetObj$analset$input <- input.2
  
  write.csv(mSetObj$analset$result, "Rarefaction.csv")
  
  return(.set.mSet(mSetObj)) 

}


#'Produce a rarefaction curve for each row of the input data. 
#'@description Produce rarefaction curve plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color options include ("darkred", "forestgreen", "hotpink", "blue") (default), "plasma" and "grayscale"
#'@param color_text Input name of the user selected colors' names. 
#'@param step step size for sample sizes in rarefaction curves, user input the number of step.
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

RarefactionCurve <- function(mSetObj=NA, step = "", color="NULL", color_text="", imgName, format="png", dpi=72, width=NA) {
  
  library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  print("Get ready for plotting")
  plot_data <- mSetObj$analset$result$Output
  sample1 <- mSetObj$analset$result$sample
  input.p <- mSetObj$analset$input
  
  #print("get richness value")
  S <- specnumber(input.2)
  
  print("width and height")
  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  print("image name")
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.Rarefaction <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  if (color=="NULL") { 
    color1 <- c("grey27", "green", "lightpink", "lightcoral", "midnightblue",
                "mediumpurple", "maroon", "lightyellow", "turquoise3", "tan3", 
                "springgreen", "thistle1", "sienna3", "orange", "dimgray",
                "burlywood", "darksalmon", "deeppink", "goldenrod", "forestgreen")
  } else if (color == "viridis") {
     color1 <- c("gray", "black", "red") 
  } else if (color == "plasma") {
     color1 <- c("green", "yellow", "blue")
  }else if (color=="manual") { #manual user entry. Selection of this option causes text box to appear
     color1 <- "manual"
     color_text1 <- color_text #colors entered by user as string with commas between colors
     color_text1 <- gsub("\n", "", color_text1, fixed=TRUE) #Prepare colors list, fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
     color_text1 <- gsub(";", ",", color_text1, fixed=TRUE)
     color_text1 <- gsub(" ", "", color_text1, fixed=TRUE)
     color_text1 <- unlist(strsplit(color_text1, split=","))
   } else { #User selected color palette
     color1 <- color #user selects color palette from drop down menu (options are grayscale, viridis, plasma)
   }
  
  pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
  
  print("set up step")
  if (step == "") {
    step1 = 1
    step1 <- as.numeric(step1)
  } else {
    step1 <- as.numeric(step) 
  }  
  
  print("actually plotting")
  #windows(width = w, height = h)
  rarecurve(input.p, step = step1, sample = sample1, col = color1,label = T, yaxt = "n")
  axis(2, las = 2, labels = T)
  
  dev.off()
 
  return(.set.mSet(mSetObj))
}




