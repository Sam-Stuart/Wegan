#'Extract column names from numeric variables-- used by ResidPlot()
#'@usage IndiceCol(mSetObj = NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
IndiceCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  #if(is.null(mSetObj$dataSet[["procr"]])){
  #  data<-mSetObj$dataSet$preproc
  #}else if(is.null(mSetObj$dataSet[["prenorm"]])){
  #  data<- mSetObj$dataSet$procr;
  #}else{
  #  data<-mSetObj$dataSet$prenorm
  #}

  metaData <- mSetObj$dataSet$origMeta

  columnsMeta <- colnames(metaData)
  #print(columnsNum)
   
  return(columnsMeta)
  
}




#'Perform species richness, evenness, diverisy index, alpha, beta & gamma density
#'@description Perform species richness, evenness, diverisy index, alpha, beta & gamma density
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param group Input the selected grouping variable, textbox default is ""
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export


div_index <- function(mSetObj = NA, data = "false", group = "NULL") {
  
  print("start model")
  options(errors = traceback)   
  #library("ade4")
  #library("adegraphics")
  #library(plyr)
  #library("dplyr")
  library("vegan")
  
  mSetObj <- .get.mSet(mSetObj)
  
  #data(dune)
  #data(dune.env)
  
  #mSetObj$dataSet$norm <- dune
  #print(mSetObj$dataSet$norm)
  #mSetObj$dataSet$origMeta <- dune.env
  metaData <- mSetObj$dataSet$origMeta
  print("set up metaData")
    
  #Extract input from mSetObj
  print("setup mSetObj")
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  print(input)

  print(colnames(metaData))

  if (group == "NULL") {
     print(group)
     group1 <- metaData[,2]
     group.name <- colnames(metaData[2])
     print("1")
  } else {
     B.frame <- metaData%>%
        select(all_of(group))
     group1 <- B.frame[,1]
     group.name <- colnames(B.frame[1])
  }
  #group1 <- colnames(metaData[,2])
  print(group1)   
  print(group.name) 
  
  cat("Alpha, beta and gamma diversity analysis works on numeric data only")
  #input.2 <- input
  
  c <- nrow(input.2)
  print(c)
  Site <- c(1:c)
  print(Site)
  

  H.shannon <- diversity(input.2, index = "shannon", base = exp(1))
  H.shannon1 <- as.data.frame(cbind(Site, "shannon", H.shannon))
  colnames(H.shannon1) <- c("Site", "Index", "ShannonIndices")
  print("H.shannon1")
   
  H.simpson <- diversity(input.2, index = "simpson")
  H.simpson1 <- as.data.frame(cbind(Site, "simpson", H.simpson))
  colnames(H.simpson1) <- c("Site", "Index", "SimpsonIndices")
  print("H.simpson1")  
    
  H.invsimpson <- diversity(input.2, index = "invsimpson")
  H.invsimpson1 <- as.data.frame(cbind(Site, "invsimpson", H.invsimpson))
  colnames(H.invsimpson1) <- c("Site", "Index", "InvsimpsonIndices")
  print("H.invsimpson1") 
    
  Fisher.alpha <- fisher.alpha(input.2)
  fisher.alpha1 <- as.data.frame(cbind(Site, "fisher.alpha", Fisher.alpha))
  colnames(fisher.alpha1) <- c("Site", "Index", "fisherAlphaIndices")
  print("fisher.alpha1")
  
  richness <- specnumber(input.2)
  richness1 <- as.data.frame(cbind(Site, richness, metaData))
  #colnames(richness1[1:2,]) <- c("Site", "Richness")
  print("richness1")
  
  richness_g <- specnumber(input.2, groups = group1)
  richness_g1 <- as.data.frame(richness_g)
  richness_g1 <- cbind(rownames(richness_g1), richness_g)
  colnames(richness_g1) <- c("Group", "Richness")

  print(group1)
  print(richness_g1)

  Evenness.shannon <- H.shannon/log(richness)
  Evenness.shannon1 <- as.data.frame(cbind(Site, Evenness.shannon))
  colnames(Evenness.shannon1) <- c("Site", "Evenness(Shannon)")
  print("Evenness.shannon1")
  
  Evenness.simpson <- H.simpson/log(richness) 
  Evenness.simpson1 <- as.data.frame(cbind(Site, Evenness.simpson))
  colnames(Evenness.simpson1) <- c("Site", "Evenness(Simpson)")
  print("Evenness.simpson1")
  
  Evenness.invsimpson <- H.invsimpson/log(richness)
  Evenness.invsimpson1 <- as.data.frame(cbind(Site, Evenness.invsimpson))
  colnames(Evenness.invsimpson1) <- c("Site", "Evenness(Invsimpson)")
  print("Evenness.invsimpson1")
    
  ShannonIndices <- as.data.frame(cbind(Site, richness, Evenness.shannon, Evenness.simpson, Evenness.invsimpson, 
                       H.shannon, H.simpson, H.invsimpson, Fisher.alpha))
  print("ShannonIndices")
  
  print("ready for alpha")
  alpha <- with(metaData, tapply(specnumber(input.2), group1, mean))
  #alpha <- with(metaData, tapply(specnumber(input.2), metaData[,group1], mean))
  alpha1 <- as.data.frame(alpha)
  alpha1 <- cbind(rownames(alpha), alpha)
  colnames(alpha1) <- c("Group", "Alpha") 
  print(alpha1)
  
  gamma <- with(metaData, specnumber(input.2, group1))
  #gamma <- with(metaData, specnumber(input.2, metaData[,group1]))
  gamma1 <- as.data.frame(gamma)
  gamma1 <- cbind(rownames(gamma1), gamma)
  colnames(gamma1) <- c("Group", "Gamma") 
  print(gamma1)
  
  #beta <- gamma/alpha - 1
  #beta1 <- as.data.frame(beta)
  #beta1 <- cbind(rownames(beta1), beta)
  #colnames(beta1) <- c("Group", "Beta") 
  
  beta_dis <- betadiver(input.2, "z")
  beta <- with(metaData, betadisper(beta_dis, group1))
  #beta <- with(metaData, betadisper(beta_dis, metaData[, group1]))
  distance <- as.numeric(beta$distances)
  #group <- as.character(beta$group)
  beta.frame <- as.data.frame(cbind(group1, distance))
  print(beta)
  print(beta.frame)  

  mSetObj$analSet$result$H_shannon <- H.shannon1
  print("1")
  mSetObj$analSet$result$H_simpson <- H.simpson1
  print("2")
  mSetObj$analSet$result$H_invsimpson <- H.invsimpson1
  print("3")  
  mSetObj$analSet$result$fisher_alpha <- fisher.alpha1
  print("4")
  mSetObj$analSet$result$richness_all_sites <- richness1
  print("5")  
  mSetObj$analSet$result$richness_group <- richness_g1
  print("6")  
  mSetObj$analSet$result$Evenness_shannon <- Evenness.shannon1
  print("7")  
  mSetObj$analSet$result$Evenness_simpson <- Evenness.simpson1
  print("8")  
  mSetObj$analSet$result$Evenness_invsimpson <- Evenness.invsimpson1
  print("9")  
  mSetObj$analSet$result$alpha <- alpha1
  print("10")  
  mSetObj$analSet$result$beta <- beta
  print("11")  
  mSetObj$analSet$result$beta_dis <- beta_dis
  print("12")  
  mSetObj$analSet$result$gamma <- gamma1
  print("13")   
  #mSetObj$analSet$result$group <- metaData[group1]
  mSetObj$analSet$result$group <- group1
  mSetObj$analSet$result$group.name <- group.name
  print("14")  
  mSetObj$analSet$input <- input.2
  print("15")    
  mSetObj$analSet$metaData <- metaData
  print("16")
  
  write.csv(mSetObj$analSet$result$H_shannon, "Shannon diversity indices.csv")
  print("17")
  write.csv(mSetObj$analSet$result$H_simpson, "Simpson diversity indices.csv")
  print("18")  
  write.csv(mSetObj$analSet$result$H_invsimpson, "Invsimpson diversity indices.csv")
  print("19")
  write.csv(mSetObj$analSet$result$fisher_alpha, "Fisher alpha diversity indices.csv")
  print("20")  
  write.csv(mSetObj$analSet$result$richness_all_sites, "Species richness all sites included.csv")
  print("21")  
  write.csv(mSetObj$analSet$result$richness_group, "Species richness by selected group.csv")
  print("22")  
  write.csv(mSetObj$analSet$result$Evenness_shannon, "Species Evenness_shannon indices.csv")
  print("23")  
  write.csv(mSetObj$analSet$result$Evenness_simpson, "Species Evenness_simpson indices.csv")
  print("24")  
  write.csv(mSetObj$analSet$result$Evenness_invsimpson, "Species Evenness_invshannon indices.csv")
  print("25")  
  write.csv(mSetObj$analSet$result$alpha, "Alpha diversity.csv")
  print("26")  
  write.csv(mSetObj$analSet$result$beta.frame, "Beta diversity.csv")
  print("27")  
  write.csv(mSetObj$analSet$result$gamma, "Gamma diversity.csv")
  print("28")

  return(.set.mSet(mSetObj))
}


#'Produce alpha diveristy of the selected group boxplot
#'@description Produce a boxplot of alpha diversity of selected groups
#'@param mSetObj Input name of the created mSet Object
#'@param color Input lines of min & max, options are "skyblue" (default), "gray", "turquoise", "slateblue", "seagreen", "wheat"  
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

AlphaPlot <- function(mSetObj=NA, color="NULL", imgName, format="png", dpi=72, width=NA) { 
  
  #library("ggplot2") 
  #library(plyr)
  #library("dplyr")
  #library(vegan)
    
  print("start alpha plot")
  mSetObj <- .get.mSet(mSetObj)
  
  alpha_p <-  as.data.frame(mSetObj$analSet$result$alpha)
  print(alpha_p)  
  Richness_data <- mSetObj$analSet$result$richness_all_sites
  print(Richness_data)  
  Richness <- Richness_data$richness
  print(Richness)  
  Group <- mSetObj$analSet$result$group
  print(Group)  
  group.name <- mSetObj$analSet$result$group.name
  print(group.name)

  #Alpha <- as.numeric(alpha_p$Alpha)
  #Group <- as.factor(alpha_p$Group)
  #metaData <- mSetObj$analSet$metaData
  #plot_data <- cbind(Richness, alpha_p)
  
  #User option to set color
  if(color == "NULL") { 
    color1 = "skyblue" #default fill palette is grayscale
  } else if (color == "gray") { 
    color1 = "gray"
  } else if (color == "turquoise") {
    color1 = "turquoise4"
  } else if (color == "slateblue") {
    color1 = "slateblue4"
  } else if (color == "seagreen") {
    color1 = "darkseagreen1"
  } else if (color == "wheat") {
    color1 = "wheat1"
  } 
  
  n <- nrow(alpha_p)
  #n1 <- c(1:n)
  #max <- max(Alpha + 5)
  m <- round(max(Richness) + 10, digits = -1)
  #se = sd(Alpha) / sqrt(length(Alpha))
  
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
  mSetObj$imgSet$pool.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  
  boxplot(Richness~Group, yaxt = "n",  
          xlab = group.name, ylab = "Alpha", col = color1, ylim = c(0, m))
  #axis(1, at = c(1:n), labels = Group)
  axis(2, las = 2)
  #at = seq(0,m), 
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}


#'Produce alpha diveristy of the selected group boxplot
#'@description Produce a boxplot of alpha diversity of selected groups
#'@param mSetObj Input name of the created mSet Object
#'@param color Input lines of min & max, options are "skyblue" (default), "gray", "turquoise", "slateblue", "seagreen", "wheat"  
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

BetaPlot <- function(mSetObj=NA, colorb="NULL", imgName, format="png", dpi=72, width=NA) { 
  
  #library(plyr)
  #library("dplyr")
  #library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  beta_p <- mSetObj$analSet$result$beta
  beta_dis_p <- mSetObj$analSet$result$beta_dis
  
  Group <- as.data.frame(beta_p$group)
  Distance <- beta_p$distances
  Group <- as.character(factor(beta_p$group))
  group.name <- mSetObj$analSet$result$group.name
  print(group.name)
  levels(Group)
  
  #User option to set color
  if(colorb == "NULL") { 
    colorb1 = "skyblue" #default fill palette is grayscale
  } else if (colorb == "gray") { #manual user entry. Selection of this option causes text box to appear
    colorb1 = "gray"
  } else if (colorb == "turquoise") {
    colorb1 = "turquoise4"
  } else if (colorb == "slateblue") {
    colorb1 = "slateblue4"
  } else if (colorb == "seagreen") {
    colorb1 = "darkseagreen1"
  } else if (colorb == "wheat") {
    colorb1 = "wheat1"
  } 
  
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
  mSetObj$imgSet$pool.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  
  boxplot(Distance~Group, yaxt = "n", xlab = group.name, ylab = "Beta", col = colorb1)
  axis(2, las = 2)
  #axis(1, at = 1:4, label = levels(Group))
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}
