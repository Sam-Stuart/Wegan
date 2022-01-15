
ColorCol <- function(mSetObj = NA) {
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
  
  columnsData <- colnames(data)
  
  return(columnsData)
  
}


VarCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data1<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data1<- mSetObj$dataSet$procr;
  }else{
    data1<-mSetObj$dataSet$prenorm
  }
  
  varData1 <- colnames(data1)
  
  return(varData1)
  
}


LineCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data3<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data3<- mSetObj$dataSet$procr;
  }else{
    data3<-mSetObj$dataSet$prenorm
  }
  
  lineData3 <- colnames(data3)
  
  return(lineData3)
  
}


Raster_data <- function(mSetObj = NA, data = "false", source = "NULL", maptype = "NULL",
                   zoom = "", var = "NULL", rangeA = "", point = "NULL", ele = "false", lineB = "NULL", polygon = "false", path = "false",
                   imgName, format = "png", dpi = 72, width = NA) {
  #library("ade4")
  #library("adegraphics")
  #library("plyr")
  #library("dplyr")
  library("ggmap")
  #library("googleway")

  mSetObj <- .get.mSet(mSetObj)

  register_google()
  
  # set default dataset as 'dune' for now
  #Extract input from mSetObj
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  
  colnames(input)[1:2] <- c("long", "lat")
  
  if (point == "NULL") {
    point.data <- input
    nameA <- colnames(point.data)[6]
    colnames(point.data)[6] <- c("Point")
    point1.data <- point.data %>%
      filter(!is.na(Point))
  } else {
    point.data <- input
    point1 <- point.data%>%
      select(all_of(point))
    nameA <- point
    colnames(point1) <- c("Point")
    point.data <- cbind(point.data, point1)
    point1.data <- point.data %>%
      filter(!is.na(Point))
  }

  if (zoom == "") {
    zoom1 <- 3
  } else {
    zoom1 <- as.numeric(zoom)
  }
  print(zoom1)
  # if the map area is too large, use smaller zoom number, e.g.: 5
  
  if (maptype == "NULL") {
    maptype1 <- "terrain"
  } else {
    maptype1 <- maptype
  }
  print(maptype1)

  if(source == "NULL") {
    source1 <- "stamen"
  } else if (source == "google") {
    source1 <- "google"
  } 
  print(source1)
  
  if (rangeA == "") {
    range1 <- 0.1
    print(range1)
  } else {
    range1 <- as.numeric(rangeA)
  }
  print(range1)
   
  if (ele == "true") {
    set_key()
    coord <- input%>%
      select(lon, lat)
    ele1 <- "NULL"
    ele1 <- google_elevation(df_locations = coord, simplify = TRUE)
    write.csv(ele1, "Elevation.csv")
    #if (ele1 == "NULL") {
    #  AddErrMsg("The length of the API query has exceeded 8192 characters and your request may not work. Try reducing the number of coordinates")
    #}
  }
  
  bbox1 <- make_bbox(lon = input$lon, lat = input$lat, f = range1)
  
  map <- get_map(location = bbox1, maptype = maptype1, zoom = zoom1, source = source1)
  #g_map <- get_googlemap(markers = state.coor, path = state.coor, scale = 1)

  print("google key")

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
  mSetObj$imgSet$ggmap <- imgName
  print(imgName)
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
    
  input.data <- as.data.frame(cbind(point1.data$long, point1.data$lat, point1.data$Point))
  print(point1.data)
  print(input.data)
  colnames(input.data) <- c("lon", "lat", "point")
  print(input.data)

  gg <- ggmap(map) +
    labs(x = "longitude", y = "latitude") +
    geom_point(mapping = aes(x = lon, y = lat, color = point), size = 2, data = input.data) 
    
  if(polygon == "true") {
    if (var == "NULL") {
      var.data <- input
      colnames(var.data)[7] <- c("Var")
      var1.data <- var.data %>%
        filter(!is.na(Var))
    } else {
      var.data <- input
      var1 <- var.data%>%
        select(all_of(var))
      colnames(var1) <- c("Var")
      var.data <- cbind(var.data, var1)
      var1.data <- var.data %>%
        filter(!is.na(Var))
    }
    input.data1 <- as.data.frame(cbind(var1.data$long, var1.data$lat, var1.data$Var, var1.data$Point))
    colnames(input.data1) <- c("lon", "lat", "var", "point")
    gg <- ggmap(map) +
      geom_polygon(data = input.data1, mapping = aes(x = lon, y = lat, group = var), fill = NA) +
      labs(x = "longitude", y = "latitude") +
      geom_point(mapping = aes(x = lon, y = lat, color = point), size = 2, data = input.data1) 
  }

    if (path == "true") {
      if (polygon == "true") {
        if (lineB == "NULL") {
          lineB.data <- input
          colnames(lineB.data)[8] <- c("Line")
          lineB1.data <- lineB.data %>%
            filter(!is.na(Line))
        } else {
          lineB.data <- input
          lineB1 <- lineB.data%>%
          select(all_of(lineB))
          colnames(lineB1) <- c("Line")
          lineB.data <- cbind(lineB.data, lineB1)
          lineB1.data <- lineB.data %>%
            filter(!is.na(Line))
        }

        input.data2 <- as.data.frame(cbind(lineB1.data$long, lineB1.data$lat, lineB1.data$Var, lineB1.data$Point, lineB1.data$Line))
        colnames(input.data2) <- c("lon", "lat", "var", "point", "line")
        gg <- ggmap(map) +
            geom_polygon(data = input.data1, mapping = aes(x = lon, y = lat, group = var1), fill = NA) +
            geom_path(data = input.data1, aes(color = line), size =3, lineend = "round") + 
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x = lon, y = lat, color = point1), size = 2, data = input.data2) 
    }
  }
  print(gg + scale_fill_discrete(name = colnames(nameA)))

  dev.off()
 
  return(.set.mSet(mSetObj))
}


