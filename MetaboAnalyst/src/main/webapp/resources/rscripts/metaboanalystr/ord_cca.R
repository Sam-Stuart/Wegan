#'Perform CCA
#'@description Perform constrained correspondence analysis between 2 data sets with the same rows
#'@param mSetObj Input name of the created mSet Object
#'@param envData Input name of environmental data set
#'@param abundance Set abundance transformation, default is absolute (no change), else relative (divide by column total)
#'@param metaData  Set meta data, default is categorical columns in data set
#'@param data Which data set to use, normalized (default) or original
#'@param env_text Input environmental data column names (java uses text box to obtain string)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.cca <- function(mSetObj=NA, envData=NA, abundance="false", data="false", env_text="NULL") {

  library("vegan")
  library("dplyr")

  #Extract input from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  if (data=="false") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  metaData <- "NULL"
  #metaData <- mSetObj$dataSet$origMeta
  envData <- mSetObj$dataSet$origEnv
  print(envData)

  #Subset main data set for cca
  num_data <- select_if(input, is.numeric) #Numeric data for CCA
  zero_sum_logic <- rowSums(num_data)>0 #Determine wich rows sum to 0
  num_data <- num_data[zero_sum_logic,] #Remove any rows with sum 0
  
  #Transform abundance data
  print("Should you have community species data, you may want to investigate the relative abundance (divide all values by column totals) versus absolute abundance (no change to data).")
  if (abundance=="false") {
    abundance1 <- "absolute"
    num_data1 <- num_data #Default abundance is absolute and no change is made to data
  } else {
    abundance1 <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
  }
  
  #Get environmental data
  envData1 <- envData #User uploaded, compulsary for constrained corerspondence analysis
  if (nrow(envData1)!=nrow(input)) {
    AddErrMsg("Your environmental data does not have the same number of rows as your main data set! Please check that the environmental data is correct.")
    stop("Your environmental data does not have the same number of rows as your main data set! Please check that the environmental data is correct.")
  }    
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env_text is second column.
  cat("Identify environmental variables for CCA using the column names with commas in between.")
  
  #Set up environmental data using user selected columns
  if (env_text=="NULL") {
    env_text1 <- colnames(envData1) #Default is the all env columns
    cat(paste0("You have selected these constraining variables: ", paste(env_text1, collapse=", "), "."))
    cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
    
  } else {
    env_text1 <- env_text #taken from text box by java, fed as string into R code
    env_text1 <- gsub("\n", "", env_text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
    env_text1 <- gsub(",", "+", env_text1, fixed=TRUE) 
    env_text1 <- gsub(";", "+", env_text1, fixed=TRUE)
    env_text1 <- gsub(" ", "", env_text1, fixed=TRUE)
    env_text1 <- gsub(":", "+", env_text1, fixed=TRUE)
    env_text1 <- gsub("*", "+", env_text1, fixed=TRUE)
    cat(paste0("You have selected these constraining variables: ", gsub("+", ", ", env_text1, fixed=TRUE), "."))
    cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
  }

  #Subset env data using user-selected columns
  #env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE)) #Create character vector for use as column names
  #print(env_cols)
  #env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)]) #Subset envData to obtain user selected environmental data
  #print(env_data)
  #print("after column crap 2")
  #env_data1 <- as.data.frame(env_data[zero_sum_logic,]) #Subset env_data removing any rows that were removed for the main data set
  #print(env_data1)
  #print("after column crap 1")
  #env_cols <- colnames(env_data1)
  #print(env_cols)
  #print("after column crap")
  
  env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE)) #Create character vector for use as column names
  print("numba 1")
  print(env_cols)
  env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)]) #Subset envData to obtain user selected environmental data
  print("numba 2")
  print(env_data)
  env_data1 <- as.data.frame(env_data[zero_sum_logic,]) #Subset env_data removing any rows that were removed for the main data set
  print("numba 3")
  print(env_data1)
  env_cols <- colnames(env_data1)
  #Subset numerical data and constraining data to remove lines containing NA
  print("numba 4")
  print(env_cols)
  env_data1 <- na.omit(env_data1)
  print("numba 5")
  print(env_data1)
  num_data1 <- num_data1[row.names(num_data1) %in% row.names(env_data1),]
  print("numba 6")
  print(num_data1)
  cca <- cca(formula=num_data1~., data=env_data1) #Run CCA with subsetted env_data

  #cca <- cca(formula=num_data1~., data=env_data1, na.action=na.omit) #Run CCA with subsetted env_data
  print(cca)
  print("after cca")
  #meta data for input, used to group samples for plotting
  if (is.data.frame(metaData)==FALSE) { #No user uplaoded meta data
    fac_data <- select_if(input, is.factor)
    count.fac.cols <- ncol(fac_data)
    if (count.fac.cols >= 1) { #If main data set had at least one categorical column, call it meta data
      metaData1 <- as.data.frame(fac_data)
    } else {
      metaData1 <- "NA" #If main data set had no categorical columns, meta data is NULL
      AddErrMsg("No groupings columns were detected. If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead ofgrouping data using 1, 2, and 3, use I, II and III.")
      print("No groupings columns were detected! If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    }
  } else {  #User uploaded meta data
    metaData1 <- metaData #User uploaded like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      AddErrMsg("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }
  
  #Fit variables to ordination plots for plotting arrows
  var_fit <- envfit(cca, num_data1, permutations=999, p.max=NULL, display="lc")
  print(var_fit)
  #Fit environmental data to ordination plots for plotting arrows and centroids
  env_data_numeric <- select_if(env_data1, is.numeric)
  print(env_data_numeric)
  env_data_factor <- select_if(env_data1, is.character)
  print(env_data_factor)
  env_fit <- envfit(cca, env_data1, permutations=999, p.max=NULL, display="lc")
  if (ncol(env_data_factor)>0) {
    env_fit_fac <- envfit(cca, env_data_factor, permutations=999, p.max=NULL, display="lc")
  }
  if (ncol(env_data_numeric)>0) {
    env_fit_num <- envfit(cca, env_data_numeric, permutations=999, p.max=NULL, display="lc")
  }
  
  #Extract row and column scores
  samp.scores <- signif(scores(cca, display="sites"), 5)
  colnames(samp.scores) <- c("CCA1", "CCA2")
  var_scores <- signif(scores(cca, display="species"), 5)
  colnames(var_scores) <- c("CCA1", "CCA2")
  
  #Extract environment scores
  if (ncol(env_data_numeric)>0) {
    env_scores.num <- signif(scores(env_fit, display="vectors"), 5)
  } else {
    env_scores.num <- "NA"
  }
  
  if (ncol(env_data_factor)>0) {
    env_scores.fac <- signif(scores(env_fit, display="factors"), 5)
  } else {
    env_scores.fac <- "NA"
  }
  
  if (is.matrix(env_scores.num)==TRUE) { #Numeric constraining variables in env data
    if (is.matrix(env_scores.fac)==TRUE) { #Categorical constraining variables in env data
      env_scores <- rbind(env_scores.num, env_scores.fac)
    } else {  #No categorical constraining variables in env data
      env_scores <- env_scores.num
    }
  } else { #No numeric constraining variables in env data
    env_scores <- env_scores.fac
  }
  colnames(env_scores) <- c("CCA1", "CCA2")
  
  #Store results in mSetObj$analSet$cca
  mSetObj$analSet$cca$name <- "cca"
  mSetObj$analSet$cca$cca <- cca
  mSetObj$analSet$cca$abundance <- abundance1
  mSetObj$analSet$cca$num_data <- num_data1
  mSetObj$analSet$cca$metaData <- metaData1
  mSetObj$analSet$cca$env_text <- env_text1
  mSetObj$analSet$cca$env_data <- env_data1
  mSetObj$analSet$cca$variable.fit <- var_fit
  mSetObj$analSet$cca$enviroment.fit <- env_fit
  mSetObj$analSet$cca$enviroment.factor.fit <- env_fit_fac
  mSetObj$analSet$cca$enviroment.numeric.fit <- env_fit_num
  mSetObj$analSet$cca$sample.scores <- samp.scores
  mSetObj$analSet$cca$var_scores <- var_scores
  mSetObj$analSet$cca$env_scores <- env_scores
  mSetObj$analSet$cca$summary <- summary(cca)
  mSetObj$analSet$cca$eigenValues <- cca$CCA$eig
  
  #Download relevent data
  write.csv(samp.scores, file="cca_sample_scores.csv", row.names=TRUE)
  write.csv(var_scores, file="cca_variable_scores.csv", row.names=TRUE)
  write.csv(env_scores, file="cca_constraining_data_scores.csv", row.names=TRUE)
  eigenValues_data <- cbind(cca$CCA$eig, cca$CCA$eig/sum(cca$CCA$eig))
  n <- nrow(eigenValues_data)
  eigenValues_data <- as.data.frame(cbind(paste0("CCA ", 1:nrow(eigenValues_data)), eigenValues_data))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  write.csv(eigenValues_data, file="cca_scree_data.csv", row.names=FALSE)
  
  sink("column_impact_on_cca.txt") 
  cat("Data columns may significantly impact constrained correspondence analysis\n")
  print(var_fit)
  sink()  
  
  sink("constraining_variables_impact_on_cca.txt") 
  cat("Constraining data may significantly impact CCA\n")
  print(env_fit)
  sink()
  
  sink("cca_summary.txt") 
  cat(paste0("Constrained Correspondence Analysis Summary\n"))
  summary(cca)
  sink()  

  return(.set.mSet(mSetObj))

}



#'Produce CCA ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce constrained correspondence analysis ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "grey"
#'@param ellipse Boolean, TRUE to add confidence ellipses, FALSE (default) to not add confidence ellipses
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param env_arrows Boolean, TRUE to produce numeric environmental data arrows, FALSE (default) to produce ordination plot without environmental arrows
#'@param env_cent Boolean, TRUE to produce display factor environmental data centroids, FALSE (default) to produce ordination plot without environmental centroids
#'@param sampleNames Boolean, TRUE to display data as variable names, FALSE (default) to display data as points
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param point_options Boolean, TRUE to turn data into points, FALSE (default) uses row names as data points
#'@param meta_col_point Meta data column to use for plotting points, Can be user inputted where options are given to java using function meta.columns()
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
Plot.CCA.biplot <- function(mSetObj=NA, color="NULL", ellipse="NULL", var_arrows="NULL", env_arrows="NULL", env_cent="NULL", sampleNames="NULL", meta_col_color="NULL", point_options="NULL", meta_col_point="NULL", imgName, format="png", dpi=72, width=NA) {

  library("vegan")
  library("viridis")

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  cca <- mSetObj$analSet$cca$cca
  metaData <- mSetObj$analSet$cca$metaData
  env_data <- mSetObj$analSet$cca$env_data
  num_data <- mSetObj$analSet$cca$num_data
  var_fit <- mSetObj$analSet$cca$variable.fit
  env_fit_fac <- mSetObj$analSet$cca$enviroment.factor.fit
  env_fit_num <- mSetObj$analSet$cca$enviroment.numeric.fit
  summary <- mSetObj$analSet$cca$summary
  
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
  mSetObj$imgSet$Plot.CCA.biplot <- imgName
  
  #Extract % variance explained by axis 1 and 2 for plotting
  CCA1 <- summary$concont$importance[2,1]
  CCA2 <- summary$concont$importance[2,2]

  #Produce constrained ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(cca, type="n", xlab=paste0("CCA 1 (", signif(CCA1, 4)*100, " %)"), ylab=paste0("CCA 2 (", signif(CCA2, 4)*100, " %)"), main="Constrained Correspondence Analysis", yaxt="n") #Empty ordination plot
  axis(2, las=2)

  #Plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    
    #point options
    if (sampleNames!="false") { #If display data as lables
      text(cca, display="sites") #Add text for samples
    } else {
      points(cca, display="sites", pch=19)
    }
    
    #Arrow options
    if (var_arrows!="false") { #If variable arrows
      plot(var_fit, col="darkred", lwd=2, p.max=NULL, cex=0.9)
    }
    
    if (env_arrows!="false") {
      plot(env_fit_num, col="blue", lwd=2, p.max=NULL, cex=0.9)
    }
    
    if (env_cent!="false") {
      plot(env_fit_fac, col="blue", lwd=2, p.max=NULL, cex=0.9)
    }
    
  } else { #If meta data available
    
    #Set up meta data column to use for colors
    if (meta_col_color=="NULL") { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User imputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }
    
    #Set up meta data column to use for points
    if (meta_col_point=="NULL") { 
      meta_col_point_data <- as.factor(metaData[,1]) #Default meta data column for labeling with points is the first
      meta_col_point_name <- colnames(metaData)[1]
    } else {
      meta_col_point_data <- as.factor(metaData[,meta_col_point]) #User imputted meta data column for labeling with points, options given to java using function meta.columns() below
      meta_col_point_name <- meta_col_point #User defined
    }
    
    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (color=="NULL") {
      colors <- viridis(n)#Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1)#Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors to points
    if (color=="none") {
      cols <- "black" #color none means all black points
    } else {
      cols <- colors[meta_col_color_data] #Grouping points Color pallete, user's choice
    }
    
    #point options
    pch_options <- c(19, 17, 15, 18, 1, 2, 0, 5, 6, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14)
    
    if (point_options=="NULL") {
      pchs <- 19 #No points option gets solid circle
    } else {
      pchs <- pch_options[meta_col_point_data] #Otherwise point options applied for grouping of user's choice
    }
    
    if (sampleNames!="false") { #If display data as lables
      with(metaData, text(cca, display="sites", col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      if (point_options!="NULL") { #Engage point options
        with(metaData, points(cca, display="sites", col=cols, pch=pchs, bg=cols)) 
        with(metaData, legend("bottomright", legend=levels(meta_col_point_data), col="black", pch=unique(pchs), pt.bg="black", title=meta_col_point_name))
      } else { #No point options
        with(metaData, points(cca, display="sites", col=cols, pch=pchs, bg=cols)) 
      }
    }
    
    #arrow options
    if (var_arrows!="false") { #If variable arrows selected
      plot(var_fit, col="darkred", lwd=2, cex=0.9)
    }
    
    if (env_arrows!="false") {
      plot(env_fit_num, col="blue", lwd=2, p.max=NULL, cex=0.9)
    }
    
    if (env_cent!="false") {
      plot(env_fit_fac, col="blue", lwd=2, p.max=NULL, cex=0.9)
    }

    #Ellipse option
    if (ellipse!="false") { #if ellipses selected
      with(metaData, ordiellipse(cca, meta_col_color_data, display="sites", kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  
  dev.off()

  return(.set.mSet(mSetObj))

}



#'Produce cca scree plot
#'@description Produce constrained correspondence analysis scree plot
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
Plot.cca.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenValues <- mSetObj$analSet$cca$eigenValues
  print("INSIDE SCREE")
  eigenValues_data <- cbind(eigenValues, eigenValues/sum(eigenValues))
  maxVar <- max(eigenValues/sum(eigenValues))

  print("after")
  print(eigenValues_data)
  n <- nrow(eigenValues_data)
  eigenValues_data <- as.data.frame(cbind(1:nrow(eigenValues_data), eigenValues_data))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  print(colnames)
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
  mSetObj$imgSet$Plot.cca.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=eigenValues_data$Dimension, y=eigenValues_data$Variance_Explained, type="l", xlim=c(1, n), ylim=c(0,maxVar+0.1), xlab="Dimension", ylab="Proportion of Variance Explained", main="Constrained Correspondence Analysis Scree Plot", xaxt="n", yaxt="n", col="blue", lwd=2)
  points(x=eigenValues_data$Dimension, y=eigenValues_data$Variance_Explained, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:n)
  dev.off()
}




##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names of meta data columns for ordination plotting'
#'@description Java will use the meta data columns to enable user options for selecting meta data for ordination plotting
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
meta.columns <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  metaData <- mSetObj$analSet$pcoa$metaData
  count.all.meta.cols <- ncol(metaData)
  name.all.meta.cols <- colnames(metaData)
  
  meta.col.results <- list(
    count=count.all.meta.cols,
    names=name.all.meta.cols
  )
  
  return(meta.col.results)
  
}



#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.ca.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.cca.result <- c(mSetObj$analSet$cca)
  return(ord.cca.result)
  
}