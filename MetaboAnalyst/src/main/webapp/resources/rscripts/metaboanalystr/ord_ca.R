#'Perform CA
#'@description Perform correspondence analysis
#'@param mSetObj Input name of the created mSet Object
#'@param fac_text Input categorical data column names (java uses text box to obtain string)
#'@param data Which data set to use, normalized (default) or original
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.ca <- function(mSetObj=NA, data="NULL", fac_text="NULL") {

  library("ade4")
  library("dplyr")
  library("FactoMineR")
  library("factoextra")

  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  input_1 <- na.omit(input)
  input <- mSetObj$dataSet$origEnv
  print(input)
  print(input_1)
  print(nrow(input))
  
  #Select categorical columns only
  print("BEFORE")
  #fac_data <- select_if(input, is.factor) #Any categorical data will be used for CA
  fac_data <- select_if(input, is.character) #Any categorical data will be used for CA
  if (ncol(fac_data)<2) {
    AddErrMsg("Less than 2 categorical columns were detected. If this is a mistake, make sure that categorical columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    stop("Less than 2 categorical columns were detected! If this is a mistake, make sure that categorical columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
  }
  #print(fac_data)
  print("INBETWEEN")
  print(fac_data)
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env_text is second column.
  cat("Identify categorical variables for correspondence analysis using the column names with commas in between.")
  
  #Set up categorical data using user selected columns
  if (fac_text=="NULL") { 
    fac_text1 <- colnames(fac_data) #Default is the all factor columns
    cat(paste0("You have selected these categorical variables: ", paste(fac_text1, collapse=", "), "."))
    cat("If the selection is not what you intended, reenter facironmental variable(s) in the text box, using the column names with commas in between.")
  } else {
    fac_text1 <- fac_text #taken from text box by java, fed as string into R code
    fac_text1 <- gsub("\n", "", fac_text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
    fac_text1 <- gsub(",", "+", fac_text1, fixed=TRUE) 
    fac_text1 <- gsub(";", "+", fac_text1, fixed=TRUE)
    fac_text1 <- gsub(" ", "", fac_text1, fixed=TRUE)
    fac_text1 <- gsub(":", "+", fac_text1, fixed=TRUE)
    fac_text1 <- gsub("*", "+", fac_text1, fixed=TRUE)
    cat(paste0("You have selected these categorical variables: ", gsub("+", ", ", fac_text1, fixed=TRUE), "."))
    cat("If the selection is not what you intended, reenter categorical variable(s) in the text box, using the column names with commas in between.")
  }
  
  #Subset environmental data set to select columns of interest
  fac_cols <- unlist(strsplit(fac_text1, "+", fixed=TRUE)) #Extract column names from fac_text1
  fac_data1 <- as.data.frame(fac_data[,which(colnames(fac_data) %in% fac_cols)]) #Subset fac_data
  colnames(fac_data1) <- fac_cols #Name columns
  
  #Perform correspondence analysis
  coa <- dudi.coa(fac_data1, scannf=FALSE)
  
  if (ncol(coa$li)==1) { #Check that at least 2 axes were detected 
    AddErrMsg("Data variance explained by one axis! Either choose other categorical columns, or try other ordination methods.")
    stop("Data variance explained by one axis! Either choose other categorical columns, or try other ordination methods.")
  }
  
  #Extract sample and variable scores
  samp.scores <- coa$li
  colnames(samp.scores) <- paste0("Axis", 1:ncol(samp.scores))
  var_scores <- coa$co
  colnames(var_scores) <- paste0("Axis", 1:ncol(var_scores))
  
  #Store results in mSetObj$analSet$rda
  mSetObj$analSet$ca$name <- "CA"
  mSetObj$analSet$ca$ca <- coa
  mSetObj$analSet$ca$fac_text <- fac_text1
  mSetObj$analSet$ca$input <- fac_data1
  mSetObj$analSet$ca$eigenValues <- coa$eig
  
  #Download relevent data
  write.csv(samp.scores, file="ca_row_scores.csv", row.names=row.names(input))
  write.csv(var_scores, file="ca_column_scores.csv", row.names=TRUE)

  eigenValues <- coa$eig
  n <- length(eigenValues)
  variance <- eigenValues/sum(eigenValues)
  eigenValues_data <- as.data.frame(cbind(paste0("CA ", 1:n), eigenValues, variance))
  colnames(eigenValues_data) <- c("Axis", "Eigen_Value", "Correlation_Coefficient")
  write.csv(eigenValues_data, file="ca_scree_data.csv", row.names=FALSE)
  
  eigenValues_summary <- as.data.frame(cbind(paste0("Axis", 1:n), eigenValues, variance))
  colnames(eigenValues_summary) <- c("Axis", "Eigen_Value", "Correlation_Coefficient")  
  sink("ca_summary.txt")
  cat("Correspondence Analysis\n")
  cat("\nCall:\n")
  print(coa[["call"]])
  cat("\nSites Scores:\n")
  print(as.data.frame(samp.scores))
  cat("\nSpecies Scores:\n")
  print(var_scores)
  cat("\nEigenvalues:\n")
  print(eigenValues_summary, row.names=FALSE)
  cat("\nRow weights:\n")
  print(coa$lw)
  cat("\nColumn weights:\n")
  print(coa$cw)
  cat("\nLengend:\n")
  cat("Site=Rows and Species=Columns\n")
  sink()  
  
  return(.set.mSet(mSetObj))
  
}





#'Produce CA 2D ordination with and without sample labels/variable arrows/color
#'@description Produce CA ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color Default (blue points and dark red arrows) or none (all black)
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param point_type Which type of points should be displayed, options include points labeled (default), text (use sample names only), points (use points and no names)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.ca.2D <- function(mSetObj=NA, color="NULL", var_arrows="NULL", point_type="NULL", imgName, format="png", dpi=72, width=NA) {

  library("FactoMineR")
  library("factoextra")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  coa <- mSetObj$analSet$ca$ca
  
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
  mSetObj$imgSet$Plot.ca.2D <- imgName
  
  #Produce
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  if (var_arrows=="NULL") { #no variable arrows (default)
    if (color=="NULL") { #Black, no color (default)
      if (point_type=="NULL") { #Default points with row name lables
        fviz_ca_row(coa, title="Correspondence Analysis\n", repel=TRUE, col.row = "black", label="row") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="text") { #Use row names instead of points
        fviz_ca_row(coa, title="Correspondence Analysis\n", repel=TRUE, col.row = "black", geom="text") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="point") { #Use points instead of row names
        fviz_ca_row(coa, title="Correspondence Analysis\n", repel=TRUE, col.row = "black", geom="point") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
    } else { #color
      if (point_type=="NULL") {
        fviz_ca_row(coa, title="Correspondence Analysis\n", repel=TRUE, col.row = "blue", label="row") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="text") {
        fviz_ca_row(coa, title="Correspondence Analysis\n", repel=TRUE, col.row = "blue", geom="text") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="point") {
        fviz_ca_row(coa, title="Correspondence Analysis\n", repel=TRUE, col.row = "blue", geom="point") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
    }
  } else { #with variable arrows
    if (color=="NULL") { #Default no color
      if (point_type=="NULL") { #Default points with row name lables
        fviz_ca_biplot(coa, title="Correspondence Analysis\n", repel=TRUE, col.col = "black", col.row = "black", label="all", arrow = c(FALSE, TRUE)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="text") { #Use row names instead of points
        fviz_ca_biplot(coa, title="Correspondence Analysis\n", repel=TRUE, col.col = "black", col.row = "black", geom="text", arrow = c(FALSE, TRUE)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="point") { #Use points instead of row names
        fviz_ca_biplot(coa, title="Correspondence Analysis\n", repel=TRUE, col.col = "black", col.row = "black", geom=c("point", "text"), label="col", arrow = c(FALSE, TRUE)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
    } else { #color
      if (point_type=="NULL") {
        fviz_ca_biplot(coa, title="Correspondence Analysis\n", repel=TRUE, col.col = "darkred", col.row = "blue", label="all", arrow = c(FALSE, TRUE)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="text") {
        fviz_ca_biplot(coa, title="Correspondence Analysis\n", repel=TRUE, col.col = "darkred", col.row = "blue", geom="text", arrow = c(FALSE, TRUE)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      } else if (point_type=="point") {
        fviz_ca_biplot(coa, title="Correspondence Analysis\n", repel=TRUE, col.col = "darkred", col.row = "blue", geom=c("point", "text"), label="col", arrow = c(FALSE, TRUE)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
    }
  }
  dev.off()
}
  
  
#'Produce CA scree plot
#'@description Produce CA stress plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.ca.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenValues <- mSetObj$analSet$ca$eigenValues
  
  n <- length(eigenValues)
  variance <- eigenValues/sum(eigenValues)
  maxVar <- variance[1]
  eigenValues_data <- as.data.frame(cbind(1:n, eigenValues, variance))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Correlation_Coefficient")

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
  mSetObj$imgSet$Plot.coa.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=eigenValues_data$Dimension, y=eigenValues_data$Correlation_Coefficient, type="l", xlim=c(1, n), ylim=c(0, maxVar+0.1), xlab="Dimension", ylab="Correlation Coefficient", main="Correspondence Analysis Scree Plot", yaxt="n", xaxt="n", col="blue", lwd=2)
  points(x=eigenValues_data$Dimension, y=eigenValues_data$Correlation_Coefficient, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:n)
  dev.off()
}



##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.ca.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.ca.result <- c(mSetObj$analSet$ca)
  return(ord.ca.result)
  
}