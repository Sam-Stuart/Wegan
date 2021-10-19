'Shapiro test for each numeric variable
#'@description For each dataset, p value from Shapiro test will be displayes as a table
#'@usage shapiroT(mSetObj = NA, data = "false")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Shiyang Zhao \email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'@export

shapiroT <- function(mSetObj = NA) {
  library(plyr)
  library(dplyr)

  mSetObj <- .get.mSet(mSetObj)
  
  #? not sure this part
  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  ## ? not sure this part
  #data <- mSetObj$dataset$norm

  a <- select_if(data, is.numeric)
  b <- select_if(data, is.character)
  b[sapply(b, is.character)] <- lapply(b[sapply(b, is.character)], 
                                         as.factor)
    
  ab.data <- data.frame()
  ab.list <- list()
  p.data <- data.frame()
  p.list <- list()  
  name.data <- data.frame()
  name.list <- list()
  test.list <- list()
  test.data <- data.frame()

  
  #c <- nlevels(b$Var)
  d <- as.numeric(ncol(a))
  e <- as.numeric(ncol(b))

  
    for(i in 1:d) {
      for (l in 1:e) {
        ab.list[[l]] <- split(data, b[,l])
        ab.data <- ab.list[[l]]
        c <- as.numeric(nlevels(b[,l]))
        for (j in 1:c) {
          test.list[[j]] <- as.data.frame(ab.data[j])
          test.data <- test.list[[j]]
          result <- shapiro.test(test.data[,i])
          name <- as.character(colnames(test.data[i]))
          p <- result$p.value
          p.list[[i]] <- data.frame(p)
          p.data <- rbind(p.data, p.list[[i]])
          name.list[[i]] <- data.frame(name)
          name.data <- rbind(name.data, name.list[[i]])
          shapiroT <- cbind(name.data, p.data)
          #shapiroT.data <- cbind(shapiroT.data, shapiroT.list[[i]])
          #shapiroT.listA[[j]] <- data.frame(shapiroT)
          #shapiroT.dataA <- rbind(shapiroT.dataA, shapiroT.listA[[j]]) 
        }
      #shapiroT.listB[[l]] <- data.frame(shapiroT.dataA)
      #shapiroT.dataB <- rbind(shapiroT.dataB, shapiroT.listB[[l]]) 
      }
    #shapiroT.listC[[l]] <- data.frame(shapiroT.dataC)
    #shapiroT.dataC <- rbind(shapiroT.dataC, shapiroT.listC[[l]]) 
    }
  
  ShapiroT.result <- as.data.frame(shapiroT)
  
  mSetObj$analset$shapiro <- ShapiroT.result
  
  return(.set.mSet(mSetObj)) 
}


'Levene's test for each numeric variable
#'@description For each dataset, p value from Levene's test will be displayes as a table
#'@usage levene(mSetObj = NA, pred.text = "NULL")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param pred.text Textbox input the variable names
#'@author Shiyang Zhao \email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'@export
levene <- function(mSetObj = NA, pred.text = "NULL"){
  
  library(car)
  library(plyr)
  library(dplyr)
  library(combinat)
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  a <- select_if(data, is.numeric)  
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  
  cb <- select_if(data, is.factor)
  
  h <- as.numeric(ncol(cb))
  g <- as.numeric(ncol(a)) 
  
  ##interaction

  if (pred.text == "NULL") {
    leve.list <- list()
    leve.data <- data.frame()
    name.list <- list()
    name.data <- data.frame()
    name_v.list <- list()
    name_v.data <- data.frame()
    for (i in 1:g) {
      for (j in 1:h) {
        #rebuild <- a %>%
        #  mutate(value <- a[i])
        value = as.numeric(a[,i])
        group = as.factor(cb[,j])
        name <- colnames(cb[j])
        name_v <- colnames(a[i])
        result <- leveneTest(value ~ group)
        p = result$`Pr(>F)`[1]
        leve.list[[i]] <- data.frame(p)
        leve.data <- rbind(leve.data, leve.list[[i]])
        name.list[[j]] <- data.frame(name)
        name.data <- rbind(name.data, name.list[[j]])
        name_v.list[[i]] <- data.frame(name_v)
        name_v.data <- rbind(name_v.data, name_v.list[[i]])
        table <- cbind(name.data, name_v.data, leve.data)
      }
    }
  } else {
    pred.text <- pred.text #taken from text box by java, fed as string into R code
  }
  
  #Currate right side of formula, and extract character vector of predictors
  pred.text <- gsub("\n", "*", pred.text, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  pred.text <- gsub(",", "*", pred.text, fixed=TRUE) 
  pred.text <- gsub(";", "*", pred.text, fixed=TRUE)
  pred.text <- gsub(" ", "*", pred.text, fixed=TRUE)
  pred.text <- gsub(":", "*", pred.text, fixed=TRUE)
  pred.text <- gsub("*", "*", pred.text, fixed=TRUE)
  
  #Subset data using predictor column names
  predictors1 <- unlist(strsplit(pred.text, "*", fixed=TRUE))
  predictors2 <- unlist(strsplit(predictors1, ":", fixed=TRUE))
  pred_data <- as.data.frame(data[,which(colnames(data) %in% predictors2)])
  
  leve_2.list <- list()
  leve_2.data <- data.frame()
  name_in.list <- list()
  name_in.data <- data.frame()
  
  for (i in 1:g) {
    #rebuild <- a %>%
    #  mutate(value <- a[i])
    value = as.numeric(a[,i])
    #name <- colnames(cb[j])
    #name_v <- colnames(a[i])
    result <- leveneTest(value ~ interaction(pred_data))
    p = result$`Pr(>F)`[1]
    leve_2.list[[i]] <- data.frame(p)
    leve_2.data <- rbind(leve_2.data, leve_2.list[[i]])
    name_in <- colnames(a[i])
    name_in.list[[i]] <- data.frame(name_in)
    name_in.data <- rbind(name_in.data, name_in.list[[i]])
    #name_v.list[[i]] <- data.frame(name_v)
    #name_v.data <- rbind(name_v.data, name_v.list[[i]])
    table_in <- cbind(pred.text, name_in.data, leve_2.data)
  }
  
  Levene.single.var.result <- as.data.frame(table) 
  Levene.inter.result <- as.data.frame(table_in)
  
  mSetObj$analset$levene.single.var <- Levene.single.var.result
  mSetObj$analset$levene.interaction <- Levene.inter.result
  
  return(.set.mSet(mSetObj)) 
  
}


'Regression model and its residuals for each numeric variable
#'@description calculating residuals of regression models
#'@usage RasidPlot(mSetObj = NA, numA, pred.text, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param numA Dynamic drop down showing numeric variables, the default is the first numeric variable
#'@param pred.text Textbox input the variable names
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Shiyang Zhao \email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'@export

ResidPlot <- function(mSetObj = NA, numA = "NULL", pred.text = "NULL", imgName, format="png", dpi=72, width=NA){
  
  library(plyr)
  library(dplyr)
   
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  
  num.data <- select_if(data, is.numeric)
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  fac.data <- select_if(data, is.factor)
  
  if (numA == "NULL") {
    numA = (num.data[,1])
  } else {
    numA <- numA  #static drow down menu - only numerical data 
  }
  
  if (pred.text=="NULL") {
    model <- lm(num.data[,1] ~ fac.data[,1])
  } else {
    pred.text <- pred.text #taken from text box by java, fed as string into R code
  }
  
  #Currate right side of formula, and extract character vector of predictors
  pred.text <- gsub("\n", "", pred.text, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  pred.text <- gsub(",", "+", pred.text, fixed=TRUE) 
  pred.text <- gsub(";", "+", pred.text, fixed=TRUE)
  pred.text <- gsub(" ", "", pred.text, fixed=TRUE)
  pred.text <- gsub(":", "+", pred.text, fixed=TRUE)
  pred.text <- gsub("*", "*", pred.text, fixed=TRUE)
  
  #Generate formula
  formula <- as.formula(paste(numA, "~", pred.text))
  #Text should be visible to user
  cat(paste0("You have created this formula for model building: ", facA, " ~ ", pred.text))
  cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
  #Subset data using predictor column names
  predictors1 <- unlist(strsplit(pred.text, "+", fixed=TRUE))
  predictors2 <- unlist(strsplit(predictors1, ":", fixed=TRUE))
  pred_data <- as.data.frame(data[,which(colnames(data) %in% predictors2)])
  model_data <- data.frame(numA, pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)
  
  data.1 <- data
  
  res <- resid(lm(formula, data = data.1))
  mSetObj$analSet$res <- res  
  mSetObj$analSet$formula <- formula
  mSetObj$analSet$pred_data <- pred_data
  mSetObj$analSet$model_data <- model_data
  mSetObj$analSet$input <- data.1
  
  return(.set.mSet(mSetObj))
  
}
  




'Residual vs. fitted plot
#'@description building a residual vs. fitted plot based on selected regression model 
#'@usage Resid_fitPlot(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  

Resid_fitPlot <- function(mSetObj = NA, imgName, format="png", dpi=72, width=NA){
  
  library(plyr)
  library(dplyr)
  
  mSetObj <- .get.mSet(mSetObj)
  res <- mSetObj$analSet$res
  input <- mSetObj$analSet$input
  fomula <- mSetObj$analSet$formula
  
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$pool.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  pars <- expand.grid(col = "black", stringsAsFactors = FALSE)
  
  plot(fitted(lm(formula, data = input)), res) 
  abline(0,0)

  dev.off()
  
  return(.set.mSet(mSetObj))

}


'Q-Q plot
#'@description building a Q-Q plot based on selected regression model 
#'@usage PlotNormSummary(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  

QQplot <- function(mSetObj = NA, imgName, format="png", dpi=72, width=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  res <- mSetObj$analSet$res
  formula <- mSetObj$analSet$formula 
  #mSetObj$analSet$pred_data <- pred_data
  #mSetObj$analSet$model_data <- model_data
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  pars <- expand.grid(col = "black", stringsAsFactors = FALSE)
  
  qqnorm(res)
  qqline(res)
  
  dev.off()
  
  return(.set.mSet(mSetObj))

}

'Density plot of residuals
#'@description building a density plot of residuals of based on selected regression model 
#'@usage PlotNormSummary(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  

Densityplot <- function(mSetObj = NA, imgName, format="png", dpi=72, width=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  res <- mSetObj$analSet$res
  formula <- mSetObj$analSet$formula 
  #mSetObj$analSet$pred_data <- pred_data
  #mSetObj$analSet$model_data <- model_data
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  pars <- expand.grid(col = "black", stringsAsFactors = FALSE)
  
  plot(density(res))
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}



