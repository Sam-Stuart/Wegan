#'Perform Rarefaction
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param type Set type for different types of rarefied community data, drop down options are "Species richness" (default), "Random rarefaction" or "Probability"
#'@param sample Input subsample size for rarefying community, either a single value or a vector.default is not apply subsample
#'@param se Input estimate standard errors, drop down options are "TRUE" or "FALSE" (default). 
#'@param MARGIN Input the index is computed, drop down options are "1" (default) or "2" 
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export


Rarefaction_div <- function(mSetObj = NA, data = "false", type = "NULL", sample = "", se = "false", MARGIN = "NULL") {
  
  #library("ade4")
  #library("adegraphics")
  library("dplyr")
  library("vegan")
  
  #Extract input from mSetObj
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }

  input <- input[order(as.numeric(row.names(input))),] #Order rows
####TESTING####
  input <- input[-1,] #Remove duplicate row
  metaData <- .readDataTable("/home/Wegan/MetaboAnalyst/src/main/webapp/resources/rscripts/metaboanalystr/test_data/dune_meta.csv") 
  #envData <- .readDataTable("/home/Wegan/MetaboAnalyst/src/main/webapp/resources/rscripts/metaboanalystr/test_data/dune_env.csv")
############


  # MARGIN = 1, data in rows; MARGIN = 2, data in columns
  if (is.na(MARGIN)) { 
    MARGIN1 <- 1
  } else {
    MARGIN1 <- 2
  }
  
  sample <- as.numeric(sample)
  if (sample == "") {
    #the minimum sample count achieved over the selected data columns
    sample1 <-  min(rowSums(BCI)) 
  } else {
    sample1 = sample
  }
  
  if (se == "false") {
    se1 = FALSE
  } else {
    se1 = TRUE
  }
  
  cat("Rarefaction technique works on numeric data only")
  input.2 <- select_if(input, is.numeric)
  
  if (type == "NULL") {
    Srare <- rarefy(input.2, sample = sample1, se = se1, MARGIN = MARGIN1)
    cat ("Expected species richness in random subsamples of size 'samples' from the community")
  } else if (type == "Random rarefaction") {
    Srare <- rrarefy(input.2, sample = sample1)
    cat("Generates one randomly rarefied community data frame or vector of given 'sample' size")
  } else if (type == "Probability") {
    Srare <- drarefy(input.2, sample = sample1)
    cat("Returns probabilities that species occur in a rarefied community of size 'sample'")  
    }
 
  mSetObj$analset$result$name <- "Rarefaction"
  mSetObj$analset$result$type <- type
  mSetObj$analset$result$sample <- sample1
  mSetObj$analset$result$Output <- Srare
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
  
  plot_data <- mSetObj$analset$result$Output
  sample1 <- mSetObj$analset$result$sample
  input.2 <- mSetObj$analset$input
  
  S <- specnumber(input.2)
  
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
  
  if(color=="NULL") { 
    color1 <- c("darkred", "forestgreen", "hotpink", "blue") #default fill palette is grayscale
  } else if (color=="manual") { #manual user entry. Selection of this option causes text box to appear
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
  
  step <- as.numeric(step)
  if (is.na(step)) {
  cat("step has to be numeric data and can't be blank")
  } else {
  step1 = as.numeric(step) 
  }  
  
  #windows(width = w, height = h)
  rarecurve(input.2, step = step1, sample = sample1, col = color1,
                          label = FALSE)
  
  dev.off()
 
  return(.set.mSet(mSetObj))
  }




