library("PBSmapping")

UTM_to_DD <- function(data) {
  a <- readline("What is the name of the Easting (X coordinate) column?")
  b <- readline("What is the name of the Northing (Y coordinate) column?")
  c <- as.integer(readline("What is the most central UTM zone for the data?"))
  data$X <- data[,a]
  data$Y <- data[,b]
  attr(data, "projection") <- "UTM"
  attr(data, "zone") <- c
  data <- convUL(data, km=FALSE)
  names(data)[names(data) == 'X'] <- "DD_Longitude"
  names(data)[names(data) == 'Y'] <- "DD_Latitude"
  return(data)
}
