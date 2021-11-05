#'Extract column names from numeric variables-- used by ResidPlot()
#'@usage AssupCol(mSetObj = NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
AssupCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  numData <- select_if(data, is.numeric)
  columnsNum <- colnames(numData)
  #print(columnsNum)
   
  return(columnsNum)
  
}

#'Shapiro test for each numeric variable
#'@description For each dataset, p value from Shapiro test will be displayes as a table
#'@usage shapiroT(mSetObj = NA, data = "false")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Shiyang Zhao \email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'@export

shapiroT <- function(mSetObj = NA, imgName, format="png", dpi=72, width=NA) {
  
  print("ShapiroT")

  #library(plyr)
  #library(dplyr)
  library(gt)

  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  #print(data)
  print("data ready")

  normData <- mSetObj$dataSet$norm
  #print(normData)
  print("Norm_transfered data ready")

  a <- select_if(normData, is.numeric)
  b <- select_if(normData, is.character)
  b[sapply(b, is.character)] <- lapply(b[sapply(b, is.character)], as.factor)
  #print(a)
  #print(b)  
  print("numerical norm_trans_data")
  print("charac norm_trans_data")
  
  a.norm <- select_if(data, is.numeric)
  b.norm <- select_if(data, is.character)
  b.norm[sapply(b.norm, is.character)] <- lapply(b.norm[sapply(b.norm, is.character)], as.factor)
  #print(a.norm)
  #print(b.norm)
  print("numerical data")
  print("chatac data")

  ab.data <- data.frame()
  ab.list <- list()
  p.data <- data.frame()
  p.list <- list()  
  name.data <- data.frame()
  name.list <- list()
  test.list <- list()
  test.data <- data.frame()

  ab.normdata <- data.frame()
  ab.normlist <- list()
  p.normdata <- data.frame()
  p.normlist <- list()  
  name.normdata <- data.frame()
  name.normlist <- list()
  test.normlist <- list()
  test.normdata <- data.frame()

  #c <- nlevels(b$Var)
  d <- as.numeric(ncol(a))
  print(d)
  e <- as.numeric(ncol(b))
  print(e)

  d.norm <- as.numeric(ncol(a.norm))
  print(d.norm)
  e.norm <- as.numeric(ncol(b.norm))
  print(e.norm)  

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
        print("shapiroT")
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
  #print(ShapiroT.result)
  
  mSetObj$analset$shapiro <- shapiroT
  print("mSetObj storage shapiroT") 

  for(g in 1:d.norm) {
      for (h in 1:e.norm) {
        ab.normlist[[h]] <- split(normData, b.norm[,h])
        ab.normdata <- ab.normlist[[h]]
        c.norm <- as.numeric(nlevels(b.norm[,h]))
        print(c.norm)
        print
        for (f in 1:c.norm) {
          test.normlist[[f]] <- as.data.frame(ab.normdata[f])
          test.normdata <- test.normlist[[f]]
          print(test.normdata)
          result.norm <- shapiro.test(test.normdata[,g])
          name.norm <- as.character(colnames(test.normdata[g]))
          print(name.norm)
          p.norm <- result$p.value
          p.normlist[[g]] <- data.frame(p.norm)
          p.normdata <- rbind(p.normdata, p.normlist[[g]])
          print(p.normdata)
          name.normlist[[g]] <- data.frame(name)
          name.normdata <- rbind(name.normdata, name.normlist[[g]])
          print(name.normdata)
          shapiroT.norm <- cbind(name.normdata, p.normdata)
          print("shapiroT.norm")
          #shapiroT.data <- cbind(shapiroT.data, shapiroT.list[[g]])
          #shapiroT.listA[[h]] <- data.frame(shapiroT)
          #shapiroT.dataA <- rbind(shapiroT.dataA, shapiroT.listA[[h]]) 
        }
      #shapiroT.listB[[l]] <- data.frame(shapiroT.dataA)
      #shapiroT.dataB <- rbind(shapiroT.dataB, shapiroT.listB[[l]]) 
      }
    #shapiroT.listC[[l]] <- data.frame(shapiroT.dataC)
    #shapiroT.dataC <- rbind(shapiroT.dataC, shapiroT.listC[[l]]) 
    }
  
  #print(shapiroT.norm)
  #print(shapiroT)

  ShapiroT.norm.result <- as.data.frame(shapiroT.norm)
  mSetObj$analset$shapiro.norm <- ShapiroT.norm.result
  print("mSetObj storage ShapiroT.norm")

  #print(ShapiroT.norm.result[2])
  ShapiroT.all.result <- cbind(ShapiroT.result, ShapiroT.norm.result[,2])
  print(ShapiroT.all.result)
  colnames(ShapiroT.all.result) <- c("Treatments", "P Value(Before Normalization)", "P Value(After Normalization)")
  print("ShapiroT.all.result, combined orig & norm")

  #ShapiroT.result.table <- data_check(ShapiroT.result)
  write.csv(ShapiroT.result, "ShapiroOrig.csv")
 
  #ShapiroT.norm.result.table <- data_check(ShapiroT.norm.result)
  write.csv(ShapiroT.norm.result, "ShapiroTranOrig.csv")

  #ShapiroT.result.table2 <- as.data.frame(round(ShapiroT.result.table, 2));
  #ShapiroT.norm.result.table2 <- as.data.frame(round(ShapiroT.norm.result.table, 2));
    
  #gt_ShapiroT.result.table <- gt(ShapiroT.result) %>% tab_options(table.font.size = 6);
  #gt_ShapiroT.norm.result.table <- gt(ShapiroT.norm.result) %>% tab_options(table.font.size = 6); 
  gt_ShapiroT.all.result.table <- gt(ShapiroT.all.result) %>% tab_options(table.font.size = 6);   
  print("get ready for plot - gt()") 

  # Fix the Formatting says Dana
  if(is.na(width)){
      w <- 10;
  }else if(width == 0){
      w <- 8;
  }else{
      w <- width;
  }
  h <- w * 1.25

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$ShapiroT <- imgName;
  #mSetObj$imgSet$ShapiroTOrig <- imgName1;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  #layout(matrix(c(1:2), nrow = 2))
  
  #par(mfrow = c(2,1))  
  #gtsave(gt_ShapiroT.result.table, filename = imgName)

  #gtsave(gt_ShapiroT.norm.result.table, filename = imgName1)

  gtsave(gt_ShapiroT.all.result.table, filename = imgName)
  print("finish shapiro") 
  #dev.off()
  return(.set.mSet(mSetObj)) 
}


#'Levene's test for each numeric variable
#'@description For each dataset, p value from Levene's test will be displayes as a table
#'@usage shapiroT(mSetObj = NA, data = "false")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Shiyang Zhao \email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'@export
levene <- function(mSetObj = NA, pred.text = "NULL", imgName, format="png", dpi=72, width=NA){
  
  print("start levene")
  library(car)
  #library(plyr)
  #library(dplyr)
  library(combinat)
  
  mSetObj <- .get.mSet(mSetObj)
  
  print("Start Levene test")

  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  #print("orig data for Levene")

  normData <- mSetObj$dataSet$norm
  #print("normData for Levene")  

  a <- select_if(data, is.numeric)  
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  cb <- select_if(data, is.factor)
  h <- as.numeric(ncol(cb))
  g <- as.numeric(ncol(a)) 
  
  a.norm <- select_if(normData, is.numeric)  
  normData[sapply(normData, is.character)] <- lapply(normData[sapply(normData, is.character)], as.factor)
  cb.norm <- select_if(normData, is.factor)
  h.norm <- as.numeric(ncol(cb.norm))
  g.norm <- as.numeric(ncol(a.norm)) 

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
        #print("Levene table orig")
      }
    }
    
    leve.norm.list <- list()
    leve.norm.data <- data.frame()
    name.norm.list <- list()
    name.norm.data <- data.frame()
    name_v.norm.list <- list()
    name_v.norm.data <- data.frame()
    for (m in 1:g.norm) {
      for (n in 1:h.norm) {
        #rebuild <- a %>%
        #  mutate(value <- a[m])
        value.norm = as.numeric(a.norm[,m])
        group.norm = as.factor(cb.norm[,n])
        name.norm <- colnames(cb.norm[n])
        name_v.norm <- colnames(a.norm[m])
        result.norm <- leveneTest(value.norm ~ group.norm)
        p.norm = result.norm$`Pr(>F)`[1]
        leve.norm.list[[m]] <- data.frame(p.norm)
        leve.norm.data <- rbind(leve.norm.data, leve.norm.list[[m]])
        name.norm.list[[n]] <- data.frame(name.norm)
        name.norm.data <- rbind(name.norm.data, name.norm.list[[n]])
        name_v.norm.list[[m]] <- data.frame(name_v.norm)
        name_v.norm.data <- rbind(name_v.norm.data, name_v.norm.list[[m]])
        table.norm <- cbind(name.norm.data, name_v.norm.data, leve.norm.data)
        #print("levene table.norm")
      }
    }
    Levene.result <- as.data.frame(cbind(table, "Before normalization"))
    Levene.norm.result <- as.data.frame(cbind(table.norm, "After normalization"))
    mSetObj$analset$levene.single.var <- Levene.result
    mSetObj$analset$levene.norm.single.var <- Levene.norm.result
    #print("mSetObj storage Levene norm & orig_single")

    Levene.all.result <- cbind(Levene.result[1:3], Levene.norm.result[3])
    print(Levene.all.result)
    colnames(Levene.all.result) <- c("Independant Variable", "Dependant Variable", "P Value(Before Normalization)", "P Value(After Normalization)")

  } else {
    pred.text <- pred.text #taken from text box by java, fed as string into R code
    print(pred.text)    

    #Currate right side of formula, and extract character vector of predictors
    pred.text <- gsub("\n", "*", pred.text, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
    pred.text <- gsub(",", "*", pred.text, fixed=TRUE) 
    pred.text <- gsub(";", "*", pred.text, fixed=TRUE)
    pred.text <- gsub(" ", "*", pred.text, fixed=TRUE)
    pred.text <- gsub(":", "*", pred.text, fixed=TRUE)
    pred.text <- gsub("*", "", pred.text, fixed=TRUE)
    print(pred.text)
  
    #Subset data using predictor column names
    predictors1 <- unlist(strsplit(pred.text, "*", fixed=TRUE))
    predictors2 <- unlist(strsplit(predictors1, ":", fixed=TRUE))
    #print(predictors1)
    #print(predictors2)
  
    pred_data.norm <- as.data.frame(normData[,which(colnames(normData) %in% predictors2)])
    pred_data <- as.data.frame(data[,which(colnames(data) %in% predictors2)])
    #print(pred_data)  
    #print(pred_data.norm)

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
        #print(table_in)
        #print("levene table interaction orig")
    }
  
    leve_2.norm.list <- list()
    print(leve_2.norm.list)
    leve_2.norm.data <- data.frame()
    name_in.norm.list <- list()
    name_in.norm.data <- data.frame()
  
    for (q in 1:g.norm) {
        value.norm = as.numeric(a.norm[,q])
        #print(value.norm)
        result.norm <- leveneTest(value.norm ~ interaction(pred_data.norm))
        #print(result.norm)
        p.norm = result.norm$`Pr(>F)`[1]
        #print(p.norm)
        leve_2.norm.list[[q]] <- data.frame(p.norm)
        leve_2.norm.list[[q]]
        leve_2.norm.data <- rbind(leve_2.norm.data, leve_2.norm.list[[q]])
        leve_2.norm.data
        name_in.norm <- colnames(a.norm[q])
        name_in.norm.list[[q]] <- data.frame(name_in.norm)
        name_in.norm.data <- rbind(name_in.norm.data, name_in.norm.list[[q]])
        table_in.norm <- cbind(pred.text, name_in.norm.data, leve_2.norm.data)
        #print(table_in.norm)
        #print("levene table interaction norm")
    }
    #print(a.norm)
    #print(g.norm)
    
    #print(table_in.norm)
    Levene.result <- as.data.frame(cbind(table_in, "Before normalization"))
    Levene.norm.result <- as.data.frame(cbind(table_in.norm, "After normalization"))
    #print(Levene.norm.result)    

    mSetObj$analset$levene.interaction <- Levene.result
    mSetObj$analset$levene.norm.interaction <- Levene.norm.result

    Levene.all.result <- cbind(Levene.result[1:3], Levene.norm.result[3])
    #print(Levene.all.result)
    colnames(Levene.all.result) <- c("Independant Variable", "Dependant Variable", "P Value(Before Normalization)", "P Value(After Normalization)")

  }
  
  #Levene.norm.result[,3:4]
  #print(Levene.norm.result[, 3:4])  

  #Levene.resultT <- t(Levene.result)
  #Levene.norm.resultT <- t(Levene.norm.result)
  #Levene.all.result <- cbind(Levene.result[1:3], Levene.norm.result[3])
  #print(Levene.all.result)
  #colnames(Levene.all.result) <- c("Independant Variable", "Dependant Variable", "P Value(Before Normalization)", "P Value(After Normalization)")

  #Levene.inter.var.all.result <- cbind(Levene.inter.result, Levene.norm.inter.var.result[,2])
  #print(Levene.inter.var.all.result)
  #colnames(Levene.inter.var.all.result) <- c("Treatments", "P Value(Before Normalization)", "P Value(After Normalization)")

  write.csv(Levene.result, "LeveneTestcsv")
  write.csv(Levene.norm.result, "LeveneTestNorm.csv")

  #write.csv(Levene.inter.var.result, "LeveneTestInteraction.csv")
  #write.csv(Levene.norm.inter.var.result, "LeveneTestInteractionNorm.csv")

  gt_Levene.all.result <- gt(Levene.all.result) %>% tab_options(table.font.size = 6);   
 
  # Fix the Formatting says Dana
  if(is.na(width)){
      w <- 10;
  }else if(width == 0){
      w <- 8;
  }else{
      w <- width;
  }
  h <- w * 1.25

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$Levene <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  #layout(matrix(c(1:2), nrow = 2))
 
  #gtsave(gt_ShapiroT.norm.result.table, filename = imgName1)

  gtsave(gt_Levene.all.result, filename = imgName)
  #gtsave(gt_Levene.Single.var.all.result, filename = imgName)
  
  #png(imgName)
  #print(p)

  #dev.off();
  print("finish Levene test")
  return(.set.mSet(mSetObj)) 
  
}



#'Regression model and its residuals for each numeric variable
#'@description calculating residuals of regression models
#'@usage RasidPlot(mSetObj = NA, numA, predText, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param numA Dynamic drop down showing numeric variables, the default is the first numeric variable
#'@param predText Textbox input the variable names
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Shiyang Zhao \email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'@export

Residuals <- function(mSetObj = NA, predText = "NULL", numA = "NULL"){
  
  #library(plyr)
  #library(dplyr)
   
  mSetObj <- .get.mSet(mSetObj)

  #print(mSetObj$dataSet)
  print("start residual calculations")

  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  #print(data)

  normData <- mSetObj$dataSet$norm
  #print(normData)

  numnormData <- select_if(normData, is.numeric)
  normData[sapply(normData, is.character)] <- lapply(normData[sapply(normData, is.character)], as.factor)
  facnormData <- select_if(normData, is.factor)  

  num.data <- select_if(data, is.numeric)
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  fac.data <- select_if(data, is.factor)
  
  #if (numA == "NULL") {
  #  numA = (num.data[1])
  #} else {
  #  numA1 <- numA  #static drow down menu - only numerical data
  #}
  #print(numA)

  #pre.inx<-GetRandomSubsetIndex(ncol(num.data), sub.num=50);
  #print(pre.inx)
  #namesVec <- colnames(num.data[,pre.inx]);
  #print(namesVec) 

  # only get common ones
  #nm.inx <- namesVec %in% colnames(numnormData)
  #print(nm.inx)
  #namesVec <- namesVec[nm.inx];
  #print(namesVec)  
  #pre.inx <- pre.inx[nm.inx];
  #print(pre.inx) 

  #norm.inx<-match(namesVec, colnames(numnormData))
  #print(norm.inx)
  #namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  #print(namesVec)  

  if (predText=="NULL") {
       
    if (numA == "NULL") {
      #nameNumA <- colnames(numA)
      #print(nameNumA)
      #nm.inx <- nameNumA %in% colnames(numnormData)
      #print("nm.inx")  
      #numB.frame <- numnormData%>%
      #  select(all_of(nameNumA))
      #print("numB.frame")
      
      modelB <- lm(num.data[,1] ~ fac.data[,1])
      resB <- resid(modelB)
      fitB <- fitted(modelB)
      print("resB")
      print(summary(modelB))
      mSetObj$analSet$resB <- resB  
      mSetObj$analSet$fitB <- fitB
      mSetObj$analSet$inputB <- data  

      modelA <- lm(numnormData[,1] ~ fac.data[,1])
      resA <- resid(modelA)
      fitA <- fitted(modelA)
      print("resA")
      print(summary(modelA))
      mSetObj$analSet$resA <- resA  
      mSetObj$analSet$fitA <- fitA
      mSetObj$analSet$inputA <- normData
    } else {
      numB.frame <- numnormData%>%
        select(all_of(numA))
      #print(numB.frame)
      numA.frame <- data%>%
        select(all_of(numA))
      #print(numA.frame)

      modelB <- lm(numB.frame[,1] ~ fac.data[,1])
      resB <- resid(modelB)
      fitB <- fitted(modelB)
      print("resB")
      print(summary(modelB))
      mSetObj$analSet$resB <- resB  
      mSetObj$analSet$fitB <- fitB
      mSetObj$analSet$inputB <- data  

      modelA <- lm(numA.frame[,1] ~ fac.data[,1])
      print("modelA")
      print(modelA)
      print(summary(modelA))
      resA <- resid(modelA)
      fitA <- fitted(modelA)
      print("resA")
      mSetObj$analSet$resA <- resA  
      mSetObj$analSet$fitA <- fitA
      mSetObj$analSet$inputA <- normData
    }

    #modelB <- lm(num.data[,1] ~ fac.data[,1])
    #resB <- resid(modelB)
    #fitB <- fitted(modelB)
    #print("resB")
    
    #mSetObj$analSet$formulaB <- formula
    #mSetObj$analSet$pred_dataB <- pred_data
    #mSetObj$analSet$model_dataB <- model_data
      

    #modelA <- lm(numB.frame[,1] ~ fac.data[,1])
    
    
    #mSetObj$analSet$formulaB <- formula
    #mSetObj$analSet$pred_dataB <- pred_data
    #mSetObj$analSet$model_dataB <- model_data
    
   
  } else {
    predText <- predText #taken from text box by java, fed as string into R code
    #print(predText)
    #Currate right side of formula, and extract character vector of predictors
    predText <- gsub("\n", "", predText, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
    predText <- gsub(",", "+", predText, fixed=TRUE) 
    predText <- gsub(";", "+", predText, fixed=TRUE)
    predText <- gsub(" ", "", predText, fixed=TRUE)
    predText <- gsub(":", "+", predText, fixed=TRUE)
    predText <- gsub("*", "*", predText, fixed=TRUE)
  
    print(predText)

    if (numA == "NULL") {
      numA <- colnames(num.data[1])
    } else {
      numA <- numA
    }
    print(numA)  

    #Generate formula
    formula <- as.formula(paste(numA, "~", predText))
    print(formula)
    #Text should be visible to user
    #cat(paste0("You have created this formula for model building: ", facA, " ~ ", predText))
    #cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
    #cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
    #Subset data using predictor column names
    #predictors1 <- unlist(strsplit(predText, "+", fixed=TRUE))
    #predictors2 <- unlist(strsplit(predictors1, ":", fixed=TRUE))
    #pred_data <- as.data.frame(data[,which(colnames(data) %in% predictors2)])
    #model_data <- data.frame(numA, pred_data)
    #colnames(model_data) <- c(paste0(facA), predictors2)
  
    data.1 <- data
    print(data.1)    

    modelB <- lm(formula, data = data.1)
    print(modelB)
    print(summary(modelB))
    resB <- resid(modelB)
    fitB <- fitted(modelB)

    modelA <- lm(formula, data = normData)
    print(modelA)
    print(summary(modelA))
    resA <- resid(modelA)
    fitA <- fitted(modelA) 

    mSetObj$analSet$resB <- resB  
    mSetObj$analSet$fitB <- fitB
    mSetObj$analSet$formula <- formula
    mSetObj$analSet$modelB <- modelB
    #mSetObj$analSet$pred_data <- pred_data
    #mSetObj$analSet$model_data <- model_data
    mSetObj$analSet$inputB <- data.1

    mSetObj$analSet$resA <- resA  
    mSetObj$analSet$fitA <- fitA
    mSetObj$analSet$formula <- formula
    mSetObj$analSet$modelA <- modelA
    mSetObj$analSet$inputA <- normData

    #print(resB)
    #print(resA)
  }

  #print(summary(lm(normData$Sepal.Length ~ normData$Species * normData$Category * normData$Cate1)))
  #print(resB)
  #print(fitB)
  #print(resA)
  #print(fitA)

  return(.set.mSet(mSetObj))
  
}
  




#'Residual vs. fitted plot
#'@description building a residual vs. fitted plot based on selected regression model 
#'@usage Resid_fitPlot(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  

ResidPlot <- function(mSetObj = NA, imgName, format="png", dpi=72, width=NA){
  
  #library(plyr)
  #library(dplyr)

  mSetObj <- .get.mSet(mSetObj)
  resB <- mSetObj$analSet$resB
  fitB <- mSetObj$analSet$fitB
  inputB <- mSetObj$analSet$inputB
  fomula <- mSetObj$analSet$formula

  resA <- mSetObj$analSet$resA
  fitA <- mSetObj$analSet$fitA
  inputA <- mSetObj$analSet$inputA
  fomula <- mSetObj$analSet$formula

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="")
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width==0){
    w = 7.2
    h = 9.5
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9
  }

  mSetObj$imgSet$residplot <- imgName
 
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  layout(matrix(c(1:6), ncol = 2))
  
  # fig 1
  op<-par(mar=c(4,7,4,0))
  plot(fitB, resB, xaxt = "n", yaxt = "n", xlab = "Fitted", cex.lab = 1.5, ylab = "", col = "black")
  axis(2, las = 2, cex.axis = 1.5) 
  axis(1, las = 1, cex.axis = 1.5)
  abline(0,0, lwd = 1.2, col = "darkblue")
  #, ylab = "Residuals",
  #, xlab = "Fitted", cex.lab = 1.5) 
  #cex.lab = 1, cex.axis = 1
  mtext("Residuals", 2, 5)
  #mtext("Fitted", 7, 10)
  mtext("Before Normalization",3, 1)

  # fig 2
  op<-par(mar=c(4,7,4,0))
  qqnorm(resB, cex.lab = 1.5, cex.axis = 1.5, main = "", yaxt = "n", ylab = "", xlab = "Theoretical Quantiles", col = "black")
  axis(2, las = 2, cex.axis = 1.5)
  #axis(1, las = 1, )
  mtext("Sample Quantiles", 2, 5)
  qqline(resB, lwd = 1.2, col = "darkblue")
  #mtext("Sample Quantiles", 2, 5)

  # fig 3
  op<-par(mar=c(4,7,4,0))
  plot(density(resB), cex.lab = 1.5, cex.axis = 1.5, lwd = 1.2, main = "", yaxt = "n", ylab = "", col = "darkblue")
  axis(2, las = 2)
  #axis(1, las = 1, )
  mtext("Density", 2, 5)
  
  # fig 4
  op<-par(mar=c(4,7,4,0), xaxt="s")
  plot(fitA, resA, cex.axis = 1.5, xaxt = "n", yaxt = "n", xlab = "Fitted", cex.lab = 1.5, ylab = "", col = "black")
  axis(2, las = 2, cex.axis = 1.5) 
  #axis(1, las = 1, )
  abline(0,0, lwd = 2, col = "darkblue")
  #mtext("Residuals", 2, 5)
  #mtext("Fitted", 1, 5)
  mtext("After Normalization",3, 1)

  # fig 5
  op<-par(mar=c(4,7,4,0), xaxt="s")
  qqnorm(resA, cex.lab = 1.5, cex.axis = 1.5, main = "", yaxt = "n", ylab = "", xlab = "Theoretical Quantiles", col = "black")
  axis(2, las = 2, cex.axis = 1.5)
  #axis(1, las = 1, )
  #mtext("Sample Quantiles", 2, 5)
  qqline(resA, lwd = 1.2, col = "darkblue")

  # fig 6
  op<-par(mar=c(4,7,4,0), xaxt="s")
  plot(density(resA), cex.lab = 1.5, cex.axis = 1.5, lwd = 1.2, main = "", yaxt = "n", ylab = "", col = "darkblue")
  axis(2, las = 2, cex.axis = 1.5)
  #axis(1, las = 1, cex.axis = 1.5)
  #mtext("Density", 2, 5)
  
  dev.off();
  
  return(.set.mSet(mSetObj))
}