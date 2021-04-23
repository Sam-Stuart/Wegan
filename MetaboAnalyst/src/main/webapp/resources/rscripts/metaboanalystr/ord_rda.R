#'Perform RDA
#'@description Perform redundancy analysis
#'@param mSetObj Input name of the created mSet Object
#'@param abundance Set abundance transformation, default is absolute (no change), else relative (divide by column total)
#'@param metaData  Set meta data, default is categorical columns in data set
#'@param envData  Set environmental data (must be user uploaded), default is none
#'@param p.max.var  Set p-value cutoff for variable fit, default is no cutoff
#'@param p.max.env  Set p-value cutoff for environmental fit, default is no cutoff
#'@param data Which data set to use, normalized (default) or original
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.rda <- function(mSetObj=NA, abundance="NULL", metaData="NULL", envData="NULL", env.text="NULL", p.max.var=0, p.max.env=0, data="NULL") { 
  
  library("vegan")
  library("dplyr")

  print("The use of data groupings will create more interesting plots. Load grouping data separately, where each row aligns with the rows in your data set, or include groupings as columns in your main data set.")
  print("The use of environmental data, if available, will also create more interesting plots. Load environmental data separately, where each row aligns with the rows in your data set.")
  
  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #Obtain numeric data for ordination and categorical data for grouping data
  num_data <- select_if(input, is.numeric)
  fac_data <- select_if(input, is.factor)
  count.fac.cols <- ncol(fac_data)
  
  print("Should you have community species data, you may want to investigate the relative abundance (divide all values by column totals) versus absolute abundance (no change to data).")
  if (abundance=="NULL") {
    abundance1 <- "absolute"
    num_data1 <- num_data #Default abundance is absolute and no change is made to data
  } else {
    abundance1 <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
  }
  
  #environmental data, used to correlate with rows in main data set
  if (is.data.frame(envData)=="NULL") { #No user uplaoded environmental data
    envData1 <- "NA"
  } else {  #User uplaoded environmental data
    envData1 <- envData #User uploaded (like weights in correlation module)
    if (nrow(envData1)!=nrow(input)) {
      #AddErrMsg("Your environmental data does not have the same number of rows as your data set! Please check that you environmental data is correct.")
      stop("Your environmental data does not have the same number of rows as your data set! Please check that you environmental data is correct.")
    }
  }
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env.text is second column.
  cat("You uploaded environmental data. Identify environmental variables for RDA using the column names with commas in between.")
  
  #Set up environmental data using user selected columns
  if (is.data.frame(envData1)==TRUE) { #User uplaoded environmental data
    if (env.text=="NULL") { #Change in other scripts
      env.text1 <- colnames(envData1) #Default is the all env columns
      cat(paste0("You have selected these constraining variables: ", paste(env.text1, collapse=", ")))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
      
    } else {
      env.text1 <- env.text #taken from text box by java, fed as string into R code
      env.text1 <- gsub("\n", "", env.text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
      env.text1 <- gsub(",", "+", env.text1, fixed=TRUE) 
      env.text1 <- gsub(";", "+", env.text1, fixed=TRUE)
      env.text1 <- gsub(" ", "", env.text1, fixed=TRUE)
      env.text1 <- gsub(":", "+", env.text1, fixed=TRUE)
      env.text1 <- gsub("*", "+", env.text1, fixed=TRUE)
      cat(paste0("You have selected these constraining variables: ", gsub("+", ", ", env.text1, fixed=TRUE), "."))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
    }
  
      env_cols <- unlist(strsplit(env.text1, "+", fixed=TRUE))
      env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)])
      colnames(env_data) <- env_cols
  } else {
    env_data <- "NA"
  }
  
  #Run RDA
  if(is.data.frame(envData1)==FALSE) { #Without envData
    rda <- rda(num_data1)
  } else{ #With user uploaded envData
    rda <- rda(X=num_data1, Y=env_data)
  }

  #meta data, used to group samples using colors or plotting symbols
  if (is.data.frame(metaData)==FALSE) { #No user uplaoded meta data
    if (count.fac.cols >= 1) { #If species data had at least one categorical column, call it meta data
      metaData1 <- as.data.frame(fac_data)
    } else {
      metaData1 <- "NA" #If species data had no categorical columns, meta data is NULL
      #AddErrMsg("No groupings columns were detected. If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead ofgrouping data using 1, 2, and 3, use I, II and III.")
      print("No groupings columns were detected! If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    }
  } else {  #User uploaded meta data
    metaData1 <- metaData #User uploaded like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your meta data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }
  
  #Fit variables to ordination plots for plotting arrows
  if (p.max.var=="NULL") { #If no p-value cutoff selected (default)
    p.max.var1 <- "NA"
    var.fit <- envfit(rda, num_data1, permutations=999, p.max=NULL, type="lc")
  } else { #If p-value cutoff selected, options are none (default), 0.05, 0.01, 0.005
    p.max.var1 <- p.max.var
    var.fit <- envfit(rda, num_data1, permutations=999, p.max=p.max.var, type="lc")
  }
  
  #Fit environmental data to ordination plots for plotting arrows
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env.fit <- "NA"
    p.max.env1 <- "NA"
  } else { #If environmental data uploaded
    if (p.max.env=="NULL") { #If no p-value cutoff selected (default)
      p.max.env1 <- "NA"
      env.fit <- envfit(rda, env_data, permutations=999, p.max=NULL, type="lc")
    } else { #If p-value cutoff selected, options are none (default), 0.05, 0.01, 0.005
      p.max.env1 <- p.max.env
      env.fit2D <- envfit(rda, env_data, permutations=999, p.max=p.max.env, type="lc")
    }
  }

  #Extract row and column scores
  samp.scores <- signif(scores(rda, display="sites"), 5)
  row.names(samp.scores) <- row.names(input)
  colnames(samp.scores) <- c("RDA1", "RDA2")
  var.scores <- signif(scores(rda, display="species"), 5)
  colnames(var.scores) <- c("RDA1", "RDA2")
  
  #Extract environment scores
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env.scores <- "NA"
  } else { #If environmental data uploaded
    env_data_numeric <- select_if(env_data, is.numeric)
    if (length(env_data_numeric)>0) {
      env.scores.num <- signif(scores(env.fit, display="vectors"), 5)
    } else {
      env.scores.num <- "NA"
    }
    
    env_data_factor <- select_if(env_data, is.factor)
    if (length(env_data_factor)>0) {
      env.scores.fac <- signif(scores(env.fit, display="factors"), 5)
    } else {
      env.scores.fac <- "NA"
    }
    
    if (is.matrix(env.scores.num)==TRUE) { #Numeric constraining variables
      if (is.matrix(env.scores.fac)==TRUE) { #Categorical constraining variables
        env.scores <- rbind(env.scores.num, env.scores.fac)
      } else {  #No categorical constraining variables
        env.scores <- env.scores.num
      }
    } else { #No numeric constraining variables
      env.scores <- env.scores.fac
    }
  }
  
  #Produce summary
  summary <- summary(rda)
    
  #Store results in mSetObj$analSet$rda
  mSetObj$analSet$rda$name <- "RDA"
  mSetObj$analSet$rda$rda <- rda
  mSetObj$analSet$rda$abundance <- abundance1
  mSetObj$analSet$rda$input <- num_data1
  mSetObj$analSet$rda$metaData <- metaData1
  mSetObj$analSet$rda$envData <- envData1
  mSetObj$analSet$rda$envDataPlotting <- env_data
  mSetObj$analSet$rda$variable.fit <- var.fit
  mSetObj$analSet$rda$enviroment.fit <- env.fit
  mSetObj$analSet$rda$p.max.env <- p.max.env1
  mSetObj$analSet$rda$p.max.var <- p.max.var1
  mSetObj$analSet$rda$sample.scores <- samp.scores
  mSetObj$analSet$rda$var.scores <- var.scores
  mSetObj$analSet$rda$env.scores <- env.scores
  mSetObj$analSet$rda$summary <- summary

  #Download relevent data
  write.csv(samp.scores, file="rda_row_scores.csv", row.names=TRUE)
  write.csv(var.scores, file="rda_column_scores.csv", row.names=TRUE)
  if (is.data.frame(envData1)==TRUE) { #If environmental data uploaded
    write.csv(env.scores, file="rda_environment_scores.csv", row.names=TRUE)
  } 

  sink("column_impact_on_rda.txt") 
  cat("Data columns may significantly impact RDA\n")
  print(var.fit)
  sink()  

  if (is.data.frame(envData1)==TRUE) { #If environmental data uploaded
    sink("environment_impact_on_rda.txt") 
    cat("Environmental data may significantly impact RDA\n")
    print(env.fit)
    sink()
  }
  
  sink("rda_summary.txt") 
  cat(paste0("Redunancy Analysis Summary\n"))
  print(summary)
  sink()  
  
  return(.set.mSet(mSetObj))
  
}





#'Produce RDA 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce RDA ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "grey"
#'@param ellipse Boolean, TRUE to add confidence ellipses, FALSE (default) to not add confidence ellipses
#'@param var.arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param env.arrows Boolean, TRUE to produce environmental arrows, FALSE (default) to produce ordination plot without environmental arrows---only appear if env data uploaded
#'@param sampleNames Boolean, TRUE to display data as variable names, FALSE (default) to display data as points
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param point.options Boolean, TRUE to turn data into points, FALSE (default) uses row names as data points
#'@param meta_col_point Meta data column to use for plotting points, Can be user inputted where options are given to java using function meta.columns()
#'@param scaling Should plot elements be scaled, options are "none" (default), "points", "arrows", "both"
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
Plot.RDA.2D <- function(mSetObj=NA, color="NULL", ellipse="NULL", var.arrows="NULL", env.arrows="NULL", sampleNames="NULL", meta_col_color="NULL", point.options="NULL", meta_col_point="NULL", scaling="NULL", imgName, format="png", dpi=72, width=NA) {
  
  library("vegan")
  library("viridis") 
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  rda <- mSetObj$analSet$rda$rda
  metaData <- mSetObj$analSet$rda$metaData
  env_data <- mSetObj$analSet$rda$envDataPlotting
  input <- mSetObj$analSet$rda$input
  var.fit <- mSetObj$analSet$rda$variable.fit
  env.fit <- mSetObj$analSet$rda$enviroment.fit
  
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
  mSetObj$imgSet$Plot.RDA.2D <- imgName
  
  #Extract % variance explained by axis 1 and 2 for plotting
  if (is.data.frame(env_data)==FALSE) {
    RDA1 <- summary$cont$importance[2,1]
    RDA2 <- summary$cont$importance[2,2]
  } else {
    RDA1 <- summary$concont$importance[2,1]
    RDA2 <- summary$concont$importance[2,2]
  }
  
  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  
  #Set scaling parameter for plotting
  if (scaling=="NULL") {
    ordiplot(rda, type="n", xlab=paste0("RDA 1 (", signif(RDA1, 4)*100, " %)"), ylab=paste0("RDA 2 (", signif(RDA2, 4)*100, " %)"), main="Redundancy Analysis", yaxt="n") #Empty ordination plot 
  } else if (scaling=="points") {
    ordiplot(rda, scaling=1, type="n", xlab=paste0("RDA 1 (", signif(RDA1, 4)*100, " %)"), ylab=paste0("RDA 2 (", signif(RDA2, 4)*100, " %)"), main="Redundancy Analysis", yaxt="n") #Empty ordination plot 
  } else if (scaling=="arrows") {
    ordiplot(rda, scaling=2, type="n", xlab=paste0("RDA 1 (", signif(RDA1, 4)*100, " %)"), ylab=paste0("RDA 2 (", signif(RDA2, 4)*100, " %)"), main="Redundancy Analysis", yaxt="n") #Empty ordination plot 
  } else if (scaling=="both") {
    ordiplot(rda, scaling=3, type="n", xlab=paste0("RDA 1 (", signif(RDA1, 4)*100, " %)"), ylab=paste0("RDA 2 (", signif(RDA2, 4)*100, " %)"), main="Redundancy Analysis", yaxt="n") #Empty ordination plot 
  }
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    
    #point options
    if (sampleNames!="NULL") { #If display data as lables
      text(rda, display="sites") #Add text for samples
    } else {
      points(rda, display="sites", pch=19)
    }
    
    #Arrow options
    if (var.arrows!="NULL") { #If variable arrows
      plot(var.fit, col="darkred", lwd=2)
    }
    
    if (env.arrows!="NULL") {
      plot(env.fit, col="blue", lwd=2)
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
      color <- "viridis" #Default
      colors <- viridis(n)#Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1)#Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assing a grey color to each level (grDevices package- automatically installed)
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
    
    if (point.options=="NULL") {
      pchs <- 19 #No points option gets solid circle
    } else {
      pchs <- pch_options[meta_col_point_data] #Otherwise point options applied for grouping of user's choice
    }
    
    if (sampleNames!="NULL") { #If display data as lables
      with(metaData, text(rda, display="sites", col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      if (point.options!="NULL") { #Engage point options
        with(metaData, points(rda, display="sites", col=cols, pch=pchs, bg=cols)) 
        with(metaData, legend("bottomright", legend=levels(meta_col_point_data), col="black", pch=unique(pchs), pt.bg="black", title=meta_col_point_name))
      } else { #No point options
        with(metaData, points(rda, display="sites", col=cols, pch=pchs, bg=cols)) 
      }
    }
    
    #arrow options
    if (var.arrows!="NULL") { #If variable arrows selected
      plot(var.fit, col="darkred", lwd=2)
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env.arrows!="NULL") { #If environment arrows selected
        plot(env.fit, col="blue", lwd=2)
      }
    }
    
    #Ellipse option
    if (ellipse!="NULL") { #if ellipses selected
      with(metaData, ordiellipse(rda, meta_col_color_data, display="sites", kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  print("DONE")
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}




#'Produce RDA scree plot
#'@description Produce RDA stress plot
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
Plot.RDA.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  summary <- mSetObj$analSet$rda$summary
  env_data <- mSetObj$analSet$rda$env_data
  
  #Extract axis importance data
  if (is.data.frame(env_data)==FALSE) {
    importance <- summary$cont$importance
  } else {
    importance <- summary$concont$importance
  }
  
  #Produce data set for plotting
  variance_data <- importance[2,]
  n <- ncol(importance)
  scree_data <- as.data.frame(cbind(1:n, variance_data))
  colnames(scree_data) <- c("RDA_Dimension", "Variance_Explained")
  write.csv(scree_data, file="redundancy_analysis_scree_data.csv", row.names=FALSE)
  
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
  mSetObj$imgSet$Plot.RDA.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=scree_data$RDA_Dimension, y=scree_data$Variance_Explained, type="l", xlim=c(1, n), ylim=c(0, 1), xlab="Number of Dimensions", ylab="Proportion of Variance Explained", main="Redundnacy Analysis Scree Plot", yaxt="n", xaxt="n", col="blue", lwd=2)
  points(x=scree_data$RDA_Dimension, y=scree_data$Variance_Explained, cex=1.1, pch=19, col="blue")
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
  
  metaData <- mSetObj$analSet$rda$metaData
  count.all.meta.cols <- ncol(metaData)
  name.all.meta.cols <- colnames(metaData)
  
  meta.col.results <- list(
    count=count.all.meta.cols,
    names=name.all.meta.cols
  )
  
  return(meta.col.results)
  
}
