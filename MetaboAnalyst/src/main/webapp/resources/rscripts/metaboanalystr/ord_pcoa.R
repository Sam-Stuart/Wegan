#'Perform PCOA
#'@description Perform principal coordinate analysis
#'@param mSetObj Input name of the created mSet Object
#'@param distance Set abundance transformation, default is absolute (no change), else relative (divide by column total)
#'@param metaData  Set meta data, default is categorical columns in data set
#'@param envData  Set environmental data (must be user uploaded), default is none
#'@param env_text Input environmental data column names (java uses text box to obtain string)
#'@param abundance Set abundance transformation, default is absolute (no change), else relative (divide by column total)
#'@param data Which data set to use, normalized (default) or original
#'@param binary Boolean, is dataset presence/absence data?, no (default), else yes
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.pcoa <- function(mSetObj=NA, data="false", distance="NULL", binary="false", abundance="false", env_text="") { #4 user inputs, plus option to upload 2 supplementary data sets
  
  options(error=traceback)

  library("vegan") #For generating distance matrix
  library("dplyr") #For easy data manipulation

  print("The use of data groupings will create more interesting plots. Load grouping data separately, where each row aligns with the rows in your data set, or include groupings as columns in your data set.")
  print("The use of environmental data, if available, will also create more interesting plots. Load environmental data separately, where each row aligns with the rows in your data set.")
  
  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="false") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  input <- input[order(as.numeric(row.names(input))),] #Order rows
  metaData <- mSetObj$dataSet$origMeta
  envData <- mSetObj$dataSet$origEnv

  #Obtain numeric data for ordination and catgroical data for grouping data
  num_data <- select_if(input, is.numeric) #Numeric data only for PCOA
  fac_data <- select_if(input, is.factor) #Any categorical data will be used for grouping
  count.fac.cols <- ncol(fac_data) #number of categorical data columns
  
  #Transform abundance data
  print("Should you have community species data, you may want to investigate the relative abundance (divide all values by column totals) versus absolute abundance (no change to data).")
  if (abundance=="false") {
    abundance1 <- "absolute"
    num_data1 <- num_data #Default abundance is absolute and no change is made to data
  } else {
    abundance1 <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
  }
  
  #Set distance measure for creation of dissimilarity matrix
  if (distance=="NULL") {
    distance1 <- "bray" #Default distance
  } else {
    distance1 <- distance #USer selected from list "bray", "manhattan", "canberra", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
  } 
  
  #Generate dissimilarity matrix
  if (binary=="false") {
    dist <- vegdist(num_data1, method=distance1, binary=FALSE) #Generate dissimilarity matrix
  } else {
    dist <- vegdist(num_data1, method=distance1, binary=TRUE) #Generate dissimilarity matrix for presence/absence data
  }
  
  #Run PCOA with and without weights
  pcoa <- wcmdscale(dist, add="lingoes", eig=TRUE, x.ret=TRUE)

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
      #AddErrMsg("Your meta data does not have the same number of rows as your numerical data! Please check that you meta data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }

  #environmental data, used to correlate with rows in main data set
  if (is.data.frame(envData)==FALSE) { #No user uplaoded environmental data
    envData1 <- "NA"
  } else {  #User uplaoded environmental data
    envData1 <- envData #User uploaded (like weights in correlation module)
    if (nrow(envData1)!=nrow(input)) {
      #AddErrMsg("Your environmental data does not have the same number of rows as your data set! Please check that you environmental data is correct.")
      stop("Your environmental data does not have the same number of rows as your data set! Please check that you environmental data is correct.")
    }
  }
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env_text is second column.
  if (is.data.frame(envData1)==TRUE) { #User uplaoded environmental data
    cat("You uploaded environmental data. Identify environmental variables for PCOA using the column names with commas in between.")
  }
  
  #Set up environmental data using user selected columns
  if (is.data.frame(envData1)==TRUE) { #User uplaoded environmental data
    if (env_text==" ") { #User doesn't specify columns-- all columns used
      env_text1 <- colnames(envData1) #Default is the all env columns
      cat(paste0("You have selected these constraining variables: ", paste(env_text1, collapse=", ")))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
      
    } else { #User enters columns
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
    
    env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE)) #Extract column names from env_text1
    env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)]) #Subset environmental data set to select columns of interest
    colnames(env_data) <- env_cols #Name columns
  } else { #No environmetal data uploaded
    env_data <- "NA"
  }
  
  #Fit environmental data to ordination plots for plotting arrows
  if (is.data.frame(env_data)==FALSE) { #If environmental data not uploaded
    env.fit <- "NA"
    env.fit.char <- "NA"
    env.fit.num <- "NA"
  } else { #If environmental data uploaded
    env_data_numeric <- select_if(env_data, is.numeric)
    env_data_character <- select_if(env_data, is.character)
    env.fit <- envfit(pcoa, env_data, permutations=999, p.max=NULL)
    if (ncol(env_data_character)>0) { #If categorical variables present
      env.fit.char <- envfit(pcoa, env_data_character, permutations=999, p.max=NULL) #Fit env data to species data
    } else{
      env.fit.char <- "NA"
    }
    if (ncol(env_data_numeric)>0) { #If numeric variables present
      env.fit.num <- envfit(pcoa, env_data_numeric, permutations=999, p.max=NULL) #Fit env data to species data
    } else{
      env.fit.num <- "NA"
    }
  }

  #Extract environment scores
  if (is.data.frame(env_data)==FALSE) { #If environmental data not uploaded
    env.scores <- "NA"
  } else { #If environmental data uploaded
    if (length(env_data_numeric)>0) { 
      env.scores.num <- signif(scores(env.fit.num, display="vectors"), 5)
    } else {
      env.scores.num <- "NA"
    }
    
    if (length(env_data_character)>0) {
      env.scores.char <- signif(scores(env.fit.char, display="factors"), 5)
    } else {
      env.scores.char <- "NA"
    }
    
    if (is.matrix(env.scores.num)==TRUE) { #Numeric constraining variables
      if (is.matrix(env.scores.char)==TRUE) { #Categorical constraining variables
        env.scores <- rbind(env.scores.num, env.scores.char)
      } else {  #No categorical constraining variables
        env.scores <- env.scores.num
      }
    } else { #No numeric constraining variables
      env.scores <- env.scores.char
    }
  }
  
  #Fit variables to ordination plots for plotting arrows
  var.fit <- envfit(pcoa, num_data1, permutations=999, p.max=NULL)
  
  #Store results in mSetObj$analSet$pcoa
  mSetObj$analSet$pcoa$name <- "PCOA"
  mSetObj$analSet$pcoa$pcoa <- pcoa
  mSetObj$analSet$pcoa$distance <- distance1
  mSetObj$analSet$pcoa$input <- num_data
  mSetObj$analSet$pcoa$eigenvalues <- pcoa$eig
  mSetObj$analSet$pcoa$var.fit <- var.fit
  mSetObj$analSet$pcoa$env.fit <- env.fit
  mSetObj$analSet$pcoa$env.fit.char <- env.fit.char
  mSetObj$analSet$pcoa$env.fit.num <- env.fit.num
  mSetObj$analSet$pcoa$metaData <- metaData1
  mSetObj$analSet$pcoa$env_data <- env_data
  
  #Output tables to save in working directory
  eigenValues_data <- cbind(pcoa$eig, pcoa$eig/sum(pcoa$eig))
  eig_rownames <- paste("PCOA ", 1:nrow(eigenValues_data), sep="")
  eigenValues_data <- cbind(eig_rownames, eigenValues_data)
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  
  write.csv(eigenValues_data, file="pcoa_eigen_values.csv", row.names=FALSE)
  write.csv(pcoa$points, file="pcoa_sample_scores.csv", row.names=row.names(input))
  write.csv(var.fit$vectors$arrows, file="pcoa_variable_scores.csv", row.names=TRUE)
  write.csv(pcoa$x, file("pcoa_dissimilarity_matrix.csv", row.names=TRUE))
  if (is.data.frame(env_data)==TRUE) {
    write.csv(env.scores, file="pcoa_constraining_data_scores.csv", row.names=TRUE)
  }

  sink("column_impact_on_pcoa.txt") 
  cat("Data columns may significantly impact PCOA\n")
  print(var.fit)
  sink() 
  
  if (is.data.frame(env_data)==TRUE) {
    sink("constraining_variables_impact_on_pcoa.txt") 
    cat("Constraining data may significantly impact PCOA\n")
    print(env.fit)
    sink() 
  }
  
  #Summary elements
  eigenValues_t <- as.data.frame(t(eigenValues_data))
  colnames(eigenValues_t) <- NULL
                
  sink("pcoa_summary.txt")
  cat("Principal Coordinate Analysis\n")
  cat("\nCall:\n")
  print(pcoa$call)
  cat(paste0("\nDistance metric:"), distance1, "\n")
  if (abundance=="absolute") {
    abundance1 <- "false"
  } else {
    abundance1 <- "true"
  }
  cat(paste0("\nPerform relative abundance transformation: "), abundance1, "\n")
  cat("\nSites Scores:\n")
  print(as.data.frame(pcoa$points))
  cat("\nSpecies Scores:\n")
  print(var.fit$vectors$arrow)
  cat("\nEnvironment Scores:\n")
  print(env.scores)
  cat("\nEigen values:")
  print(eigenValues_t)
  cat(paste0("\nAdditive constant calculated using Lingoes procedure for negative eigen value correction: ", pcoa$ac, "\n"))
  cat("\nLengend:\n")
  cat("Site=Rows and Species=Columns\n")
  sink()  
  
  return(.set.mSet(mSetObj))
  
}





#'Produce PCOA 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce PCOA ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "grey"
#'@param ellipse Boolean, TRUE to add confidence ellipses, FALSE (default) to not add confidence ellipses
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param env_arrows Boolean, TRUE to produce environmental arrows, FALSE (default) to produce ordination plot without environmental arrows---only appear if env data uploaded
#'@param env_cent Boolean, TRUE to produce display factor environmental data centroids, FALSE (default) to produce ordination plot without environmental centroids
#'@param sampleNames Boolean, TRUE to display data as variable names, FALSE (default) to display data as points
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
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
Plot.PCOA.2D <- function(mSetObj=NA, ellipse="false", var_arrows="false", env_arrows="false", env_cent="false", sampleNames="false", color="NULL", meta_col_color="NULL", imgName, format="png", dpi=72, width=NA) { #5 check boxes, 3 drop downs
  
  options(error=traceback)

  library("vegan")
  library("viridis") 
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  pcoa <- mSetObj$analSet$pcoa$pcoa
  metaData <- mSetObj$analSet$pcoa$metaData
  input <- mSetObj$analSet$pcoa$input
  var.fit <- mSetObj$analSet$pcoa$var.fit
  env.fit.char <- mSetObj$analSet$pcoa$env.fit.char
  print(env.fit.char)
  env.fit.num <- mSetObj$analSet$pcoa$env.fit.num
  print(env.fit.num)
  env_data <- mSetObj$analSet$pcoa$env_data
  print(env_data)

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
  mSetObj$imgSet$Plot.PCOA.2D <- imgName
  
  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  ordiplot(pcoa$points, type="n", main="Principal Coordinate Analysis", yaxt="n", xlab="PCOA 1", ylab="PCOA 2", choices=c(1,2)) #Empty ordination plot 
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no meta data

    #point options
    if (sampleNames!="false") { #If display data as lables
      text(pcoa$points) #Add text for samples
    } else {
      points(pcoa$points, pch=19, col="black") #Add text for samples, all black since grouping data is unavailable
    }
    
    #variable arrow options
    if (var_arrows!="false") { #If variable arrows selected
        if (color=="plasma") {
            plot(var.fit2D, col="black")
        } else {
            plot(var.fit2D, col="darkred") 
        }
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows!="false") { #If environment arrows selected
        plot(env.fit.num, col="blue", lwd=2)
      }
      
      if (env_cent!="false") { #If environment constraints selected
        plot(env.fit.char, col="blue", lwd=2)
      }
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
      cols <- "black" #color none means black points
    } else {
      cols <- colors[meta_col_color_data] #Color pallete applied for groupings of user's choice
    }
    
    if (sampleNames!="false") { #If display data as lables
      with(metaData, text(pcoa$points, col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      with(metaData, points(pcoa$points, col=cols, pch=19, bg=cols)) 
    }
    
    #Arrow options
    if (var_arrows!="false") { #If variable arrows selected
      plot(var.fit, col="darkred", lwd=2)
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows!="false") { #If environment arrows selected
        plot(env.fit.num, col="blue", lwd=2)
      }
      
      if (env_cent!="false") { #If environment constraints selected
        plot(env.fit.char, col="blue", lwd=2)
      }
    }
    
    #Ellipse option
    if (ellipse!="false") { #if ellipses selected
      with(metaData, ordiellipse(pcoa, meta_col_color_data, kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


#'Produce PCOA 3D ordination plot
#'@description Rotate PCOA analysis
#'@usage PlotPCOA3DScore(mSetObj=NA, imgName, format="json", inx1, inx2, inx3)
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "cividis"
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param imgName Input the image name
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

Plot.PCOA.3D <- function(mSetObj=NA, color="NULL", var_arrows="false", meta_col_color="NULL", imgName){
    
  options(error=traceback)

  library("viridis")
  library("RJSONIO")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$analSet$pcoa$input
  pcoa <- mSetObj$analSet$pcoa$pcoa
  metaData <- mSetObj$analSet$pcoa$metaData

  #Create list to hold plot items
  pcoa3D_plot <- list()
  
  #Samples (rows)
  pcoa3D_plot$score$axis <- paste("PCOA", c(1, 2, 3), sep="")
  sampleCoords <- data.frame(t(as.matrix(pcoa$points[,1:3])))
  colnames(sampleCoords) <- NULL
  pcoa3D_plot$score$xyz <- sampleCoords
  pcoa3D_plot$score$name <- rownames(input)
  
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    pcoa3D_plot$score$color <- "NA"
    pcoa3D_plot$score$point <- "NA"
  } else { #If meta data
    
    #Set up meta data column to use for colors
    if (meta_col_color=="NULL") { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User imputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }

    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (is.null(color)) {
      color <- "viridis" #Default
      colors <- viridis(n) #Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1) #Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assing a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors
    if (color=="none") {
      cols <- "black"
    } else {
      cols <- colors[meta_col_color_data]
    }
    pcoa3D_plot$score$color <- col2rgb(cols)
    
  }
  
  #Variables (columns)
  if (var_arrows=="false") {
    pcoa3D_plot$scoreVar$axis <- "NA"
    pcoa3D_plot$scoreVar$xyzVar <- "NA"
    pcoa3D_plot$scoreVar$nameVar <- "NA"
    pcoa3D_plot$scoreVar$colorVar <- "NA"
  } else {
    #3D debug start
    print(var.scores3D)
    #3D debug end
    variableCoords <- data.frame(t(as.matrix(var.scores3D)))
    colnames(variableCoords) <- NULL
    pcoa3D_plot$scoreVar$axis <- paste("PCOA", c(1, 2, 3), sep="")
    pcoa3D_plot$scoreVar$xyzVar <- variableCoords
    pcoa3D_plot$scoreVar$nameVar <- colnames(input)
    if (color=="plasma") {
      pcoa3D_plot$scoreVar$colorVar <- col2rgb("black")
    } else {
      pcoa3D_plot$scoreVar$colorVar <- col2rgb("darkred")
    }
  }

  #From metaboanalyst, unclear what it does
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])))
  }else{
    cls <- as.character(mSetObj$dataSet$cls)
  }

  if(all.numeric(cls)){
    cls <- paste("Group", cls)
  }

  pcoa3D_plot$score$facA <- cls
  
  imgName <- paste(imgName, ".json", sep="")
  json.obj <- RJSONIO::toJSON(pcoa3D_plot, .na='null')
  sink(imgName)
  cat(json.obj)
  sink()
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj))
  }
}




#'Produce PCOA scree plot
#'@description Produce PCOA scree plot
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
Plot.PCOA.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
    
  options(error=traceback)

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenvalues <- mSetObj$analSet$pcoa$eigenvalues
  
  #Produce data set for plotting
  eigenValues_data <- eigenvalues[1:6]
  eigenvalues_data <- as.data.frame(cbind(1:6, eigenValues_data/sum(eigenvalues)))
  maxVar <- eigenvalues_data[1,2]
  
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
  mSetObj$imgSet$Plot.PCOA.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=eigenvalues_data$V1, y=eigenvalues_data$V2, type="l", xlim=c(1, 6), ylim=c(0, maxVar+0.1), xlab="Dimension", ylab="Proportion of Variance Explained", main="Principal Coordinate Analysis Scree Plot", yaxt="n", xaxt="n", col="blue", lwd=2)
  points(x=eigenvalues_data$V1, y=eigenvalues_data$V2, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:6)
  dev.off()
}



#'Produce PCOA stress plot
#'@description Produce PCOA stress plot (stress is the mismatch between the rank order of distances in the data, and the rank order of distances in the ordination)
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
Plot.PCOA.stress <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
    
  options(error=traceback)

  library("vegan")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  pcoa <- mSetObj$analSet$pcoa$pcoa

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
  mSetObj$imgSet$Plot.PCOA.stress <- imgName
  
  #Stress plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) #No plotting outside of plot limits, set margins to default
  vegan::stressplot(pcoa, main="Principal Coordinate Analysis\nShepard Plot for Goodness of Fit", yaxt="n")
  axis(2, las=2) #yaxis numbers upright for readability
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
pcoa.meta.columns <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  metaData <- mSetObj$analSet$pcoa$metaData
  name.all.meta.cols <- colnames(metaData)

  return(name.all.meta.cols)
  
}


#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.pcoa.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.pcoa.result <- c(mSetObj$analSet$pcoa)
  return(ord.pcoa.result)
  
}