################################################################################
#
# preprocessing_ClusterANalysis.r
#
# purpose: prepare input data for further analysis in R
#
# author: sven.lautenbach@ufz.de, tomas.vaclavik@ufz.de
#
#
################################################################################

################################################################################
# log function
myLog <- function(...) {
	cat(paste0("[glues.systemarchetypes] ", Sys.time(), " | ", ..., "\n"))
}
myLog("Start script PREPROCESSING ... ")

################################################################################
# for testing
#wps.off;
setwd("D:/Dokumente/2014_GLUES/WPS4R/System-Archetypes/")
#wps.on;

################################################################################
require(sp)
require(raster)

## wps.des: id = glues.systemarchetypes.preprocessing, title = preprocessing for 
## system archetypes, abstract = process loads input files and saves them in 
## one data frame;

# wps.resource: Input_Data_v2;

# wps.in: id = dataPath, type = string,
# title = the path to the data layers, minOccurs = 0, maxOccurs = 1,
# abstract = "the path to the data layers, must be a local server path",
# value = Input_Data_v2;
#wps.off;
dataPath <- "Input_Data_v2"
#wps.on;

# the order of the files matters, since the conversion to SpatialPixelsDataFrame
# to do the sampling does not work for all raster layers...
files <- c("crop2005",
					 "cropdif50",
					 "grass2005",
					 "grassdif50",
					 "nfert",
					 "irrigation",
					 "totsederosion",
					 "y_wheat",
					 "y_maize",
					 "y_rice",
					 "yg_wheat",
					 "yg_maize",
					 "yg_rice",
					 "tpi_agr",
					 "hanpp",
					 "bio1",
					 "bio2",
					 "bio12",
					 "bio15",
					 "bio21",
					 "temp_anom",
					 "ndvi_mean",
					 "ndvi_sd",
					 "soil_orgc",
					 "spec_rich",
					 "gdp",
					 "agr_gdp",
					 "gcs_agr",
					 "pop_glds00ag",
					 "popdif50",
					 "polstability",
					 "accessibility") 
myLog("Input datasets: ", toString(files))

# wps.in: id = sampleSize, type = integer, minOccurs = 0, maxOccurs = 1,
# title = size of the sampling,
# abstract = the number of sampling cells that are used in spsample,
# value = 1000000;
# wps.in: id = samplingType, type = string, minOccurs = 0, maxOccurs = 1,
# title = sampling strategy,
# abstract = "strategy of the sampling used in spsample (e.g. random, regular, 
# stratified, nonaligned, hexagonal, clusted or Fibonacci)",
# value = regular;
#wps.off;
sampleSize <- 100 #1*10^6
samplingType <- "regular"
#wps.on;

myLog("Sample size: ", sampleSize, " | sampling type: ", samplingType)

myLog("Workspace: ", getwd())

#wps.off;
#testRaster <- raster(paste0(dataPath, files[10]))
#testRaster
#summary(testRaster)
#as(testRaster, "SpatialPixelsDataFrame")
# DN: ERROR, happens with several rasters I tested...
# Error in as.factor(f[[i]][v[, i]]) : 
#Fehler bei der Auswertung des Argumentes 'x' bei der Methodenauswahl
#für Funktion 'as.factor': Error in `[.data.frame`(f[[i]], v[, i]) : undefined columns selected
#wps.on;

################################################################################
# 
rasterList <- list()

for (currentFile in files) {
  fileName <- paste0(dataPath, "/", currentFile)
  # testing: fileName <- paste0(dataPath, "/", "crop2005")
  myLog("Processing ", fileName, " - file exists: ", file.exists(fileName))
  
  raster <- raster(fileName)
  myLog("Current raster: ", toString(capture.output(summary(raster))))
  
  # add raster to the raster list
  rasterList[[length(rasterList)+1]] <- raster 
  
  # if this is the first raster, run the sampling
  if(!exists("theDF")) {
    first <- FALSE
    myLog("Running sampling based on raster ", names(raster), " from file ",
    			fileName)
    
    # sample data
    # the aim of this step is twofold:
    #   1) reduce the numberof datapoints for the analysis
    #   2) reduce the problem of spatial auto correlation
 
    spdf <- as(raster, "SpatialPixelsDataFrame")
    summary(spdf)
    sampelPixels <- spsample(spdf, n = sampleSize, type = samplingType)
    samplePoints <- as(sampelPixels, "SpatialPoints")
    theDF <- cbind(coordinates(samplePoints))
  }
  
  rasterSampled <- extract(raster, samplePoints) 
  theDF <- cbind(theDF, rasterSampled)
}

# convert to data.frame
theDF <- as.data.frame(theDF)
names(theDF) <- c("x", "y", files)
myLog("Create data frame with size ", toString(dim(theDF)), " and variables ",
			toString(names(theDF)))

sampleBaseDataStatistics <- "sample-data_statistics.txt"
capture.output(summary(theDF), file = sampleBaseDataStatistics)
# wps.out: sampleBaseDataStatistics, type = text, 
# title = statistics of dataset,
# abstract = the output of a summary operation on the created data.frame;
myLog("Saved summary statistics in file ", sampleBaseDataStatistics)


sampledBaseData <- paste0("inputData_", as.integer(sampleSize), "_",
													samplingType, ".Rdata")
save(theDF, file = sampledBaseData)
# wps.out: sampledBaseData, type = rdata, title = sampled datasets,
# abstract = an R data.frame with the sampled input data for further analysis;
myLog("Saved sampled data in file ", sampledBaseData, " in directory ", getwd(),
			" | file size: ",	file.info(sampledBaseData)$size / (1024*1024), " MB")

myLog("Done!")
