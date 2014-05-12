# <---Single hast comments (other than this one) are from the original script provided by Abby 
# before I exploded it into a much larger file. I commented alot. -Matt
## <--Double hash comments are my verbose additions to this file.

## Load taxize library
getPackage <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
  return(TRUE)
}
getPackage("taxize")
getPackage("plyr")

## Get tsns and remove any records that do not return a tsn

## Declare column because R doesn't like doing that inside a for() loop
lut_species$tsn <- ""

## Function containing for() loop to get TSN line by line through the dataset
getTSN <- function(lut) {
  for (i in 1:nrow(lut)) {
    if (nchar(lut[i,ncol(lut)]) == 0) {
      lut_species[i,]$tsn <<- get_tsn(lut[i,]$scientificName_clean, searchtype = "sciname", ask = TRUE, verbose = TRUE)   
    }}
}

## Catches errors in previous function (server timeout, no response from server, etc...) and
## effectively restarts the function with the error catching enabled again. 
TSNstart <- function() {
  tryCatch(getTSN(lut_species), error = function(e) TSNstart())
}

## Starts the for() and error catching loops above
TSNstart()

## Subset species list to those records that returned a TSN 
## Those that do not have TSN will break further steps)
dat <- subset(lut_species,lut_species$tsn != "NA")
dat$scientificName <- NULL
dat <- unique(dat)
row.names(dat) <- NULL

# Get classification data
## Declare column because R doesn't like doing that inside a for() loop
out <- ""

## Function containing for() loop to get taxonomy info line by line through the dataset
getTAX <- function(x) {
  for (i in 1:nrow(x)) {
    if (length(out) <= i) {
      tsn <- x[i,]$tsn
      class(tsn) <- "tsn"
      out[x[i,]$tsn] <<- classification(tsn, db = "itis")
    }}
}

## Catches errors in previous function (server timeout, no response from server, etc...) and
## effectively restarts the function with the error catching enabled again.
TAXstart <- function() {
  tryCatch(getTAX(dat), error = function(e) TAXstart())
}

## Starts the for() and error catching loops above
TAXstart()

## Declaring "out" above caused the first list element to be an empty string.
## This removes that first empty string element so that later functions don't break.
out<- out[2:length(out)]

## The 'classification' class is usually generated when classification() is performed on an entire list
## instead of on individual elements at a time. This code forces that class on the finished list.
class(out) <- 'classification'

## coerces list into dataframe
out <- ldply(out, function(x){
  ss <- x$name
  names(ss) <- x$rank
  data.frame(t(ss), stringsAsFactors = FALSE)
})
names(out)[1] <- "tsn"

# Remove unnecessary columns
out$Subkingdom <- NULL
out$Infrakingdom <- NULL
out$Subphylum <- NULL
out$Infraphylum <- NULL
out$Superclass <- NULL
out$Subclass <- NULL
out$Infraclass <- NULL
out$Superorder <- NULL
out$Suborder <- NULL
out$Superfamily <- NULL
out$Superphylum <- NULL
out$Subdivision <- NULL
out$Infradivision <- NULL
out$Subfamily <- NULL
out$Infraorder <- NULL
out$Tribe <- NULL
out$Row.names <- NULL

## Add a TaxonRank information. This method takes out the extra call on the ITIS servers to get TaxonRank info.
## Reorder columns for hierarchical compare. Add "Subspecies" to this list if the dataset doesn't contain it.
out <- out[c("tsn","Kingdom","Phylum","Class","Order","Family","Genus","Species")]
out$TaxonRank <- names(out[apply(out, 1, function(x) {max(which(!is.na(x)))})])
out$TaxonRank <- gsub("[0-9]","",out$TaxonRank)
out$TaxonRank <- gsub("[.]","",out$TaxonRank)

# Merge Phylum and Division so it's just one column
out[is.na(out)] <- ""
out$Phylum <- paste(out$Phylum, out$Division)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
out$Phylum <- trim(out$Phylum)
out$Division <- NULL

## Join original species and TSN information with taxonomic hierarchy information, based on scientific name
dat2 <- merge(dat, out, by = "tsn", all=TRUE)
dat2$tsn <- as.numeric(dat2$tsn)

## Declare column because R doesn't like doing that inside a for() loop
dat2$Author <- ""

## Function containing for() loop to get taxonomy info line by line through the dataset
getAUTH <- function(x) {
  for (i in 1:nrow(x)) {
    if (nchar(x[i,ncol(x)]) == 0) {
      a <- itis_getrecord(x[i,]$tsn, by = "tsn")
      dat2[i,ncol(x)] <<- a$scientificName$author 
    }}
}

## Catches errors in previous function (server timeout, no response from server, etc...) and
## effectively restarts the function with the error catching enabled again.
AUTHstart <- function() {
  tryCatch(getAUTH(dat2), error = function(e) AUTHstart())
}

## Starts the for() and error catching loops above
AUTHstart()

## Some Author searches in ITIS return "true" as a string value. This line replaces that with an empty string
dat2$Author <- str_replace_all(dat2$Author, "true", "")

#Remove genus from species name (probably an OBIS specific task)
dat2$Species <- gsub("[A-z]* ","", dat2$Species)


## At this point "dat" has all the info needed, but lut_species is still necessary to keep up with rows that 
## did not return at tsn during the original tsn search, so I remove the duplicate columns from lut_species 
## for the merge so the total row number of the final lut_species file matches the original input file.
dat2$tsn <- NULL
dat2$fresh_marine <- NULL
lut_species <- merge(lut_species, dat2, by = "scientificName_clean", all.x = TRUE)


## Clean up unnecessary files from environment (comment out these rm() functions if you want to view intermediate files)
#rm(dat)
#rm(dat2)
#rm(trim)
#rm(out)
#rm(a)
#rm(tsn)
#rm(i)
