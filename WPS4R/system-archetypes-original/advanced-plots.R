# test file for creating the output maps

# Base code by Tomas Vaclavik <tomas.vaclavik@ufz.de>

# wps.off;
# load feature space data (the one saved into the shapefile)
setwd("C:/Users//Daniel/Documents/2014_GLUES/WPS4R/System-Archetypes/")

# load the data
#source("full-process.R")
load(file = "C:/Users/Daniel/Documents/2014_GLUES/WPS4R/System-Archetypes/sysarch-som_4_4_hexagonal_sample-976997_run-1.Rdata")
#.som.fs <- systemArchetypesData

# run functions
mapData <- prepareData(systemArchetypesData)
saveMaps(mapData)
savePlots(codeVectors, systemArchetypesData)

myLog <- function(...) {
    cat(paste0("[glues.systemarchetypes] ", Sys.time(), " | ", ..., "\n"))
}
# wps.on;


prepareData <- function(data) {
    require(sp)
    
    plotData <- data
    coordinates(plotData) <-  ~x+y
    #str(plotData)
    
    myLog("Prepared data: ", paste(capture.output(summary(plotData)), sep = "\n", collapse = "\n"))
    return(plotData)
}

saveMaps <- function(plotData, lsaMapName = "lsa-map-figure-1.png", distanceMapName = "som-distance-map-figure-A2.png",
                     width = 1280, height = 720) {
    require(raster)
    require(rasterVis)
    require(RColorBrewer) # display.brewer.all()
    require(rworldmap)
    data("countriesLow")
    
    myLog("Creating and saving maps")
    
    ###############################################################################
    # Convert output shapefile to grid (to make final map)
    # - convert point shapefile (samples) to a grid layer
    # - fill in the entire extent by assigning each grid cell the value of its 
    #   closest sample point (basically a nearest neigbor algorithm - using the 
    #   'nibble' tool in ArcGIS)
    # - map created manually in ArcGIS with the appropriate symbology etc.
    
    # create grid layer based on .som.fs$som.unit <- som.result$unit.classif instead of loading shapefile
    
    # create rasterized map
    rast <- raster()
    extent(rast) <- extent(plotData)
    aspectRatio <- (extent(plotData)@xmax - extent(plotData)@xmin) / (extent(plotData)@ymax - extent(plotData)@ymin)
    nrow(rast) <- 1000
    ncol(rast) <- round(nrow(rast) * aspectRatio)
    # see what happens by couting the number of points in each cell
    #plotDataRaster <- rasterize(plotData, rast, fun = "count", field = "som.unit")
    
    myLog("Start rasterize...")
    plotDataRaster <- rasterize(plotData, rast, fun = mean, field = "som.unit", na.rm = TRUE)
    plotDataRaster <- ratify(plotDataRaster, count = TRUE) # change to categorical raster
    rat <- levels(plotDataRaster)[[1]]
    rat$class <- paste("LSA", c(1:numberOfCategories)) # rename categories
    levels(plotDataRaster) <- rat
    
    #nrow(plotDataRaster) * ncol(plotDataRaster)
    #sort(unique(plotData[["som.unit"]]))
    numberOfCategories <- dim(rat)[[1]]
    myLog("Done. Found ", numberOfCategories, " categories")
    myLog("Levels:\n", paste(capture.output(levels(plotDataRaster)), sep = "\n", collapse = "\n"))
    
    # Figure 1
    myLog("Saving Figure 1...")
    png(filename = lsaMapName, width = width, height = height)
    lsaMap <- levelplot(plotDataRaster, main = "Global land use archetypes",
                        col.regions = sample(rainbow(numberOfCategories), size = numberOfCategories),
                    att = "class", sp.layout = list("sp.polygons", countriesLow, col = "gray50", first = FALSE))
    #histogram(plotDataRaster)
    print(lsaMap)
    dev.off()
    
    # Figure A2 - plot using som.result$distance
    myLog("Saving Figure A2...")
    png(filename = distanceMapName, width = width, height = height)
    myLog("Distance statistics:\n", paste(capture.output(summary(plotData$som.distance)), sep = "\n", collapse = "\n"))
    plotDistanceRaster <- rasterize(plotData, rast, fun = mean, field = "som.distance", na.rm = TRUE)
    distanceMap <- levelplot(plotDistanceRaster, col.regions = rev(bpy.colors()), zscaleLog = TRUE,
                             main = "Distance to cluster (log scale)",
        sub = "Quality assessment of the classification procedure. The map displays the distance of each grid cell,
       \n mapped to a particular cluster, to the codebook vector of that cluster. Low values indicate good quality of mapping.")
    print(distanceMap)
    dev.off()
    
    myLog("Done with map plots, saved to ", lsaMapName, " and ", distanceMapName)
}


savePlots <- function(vectors, data, lsaBarplotFilename = "lsa-barplot.png", width = 720, height = 1280) {
    ###############################################################################
    # Calculate statistics (to plot Fig. 3 and A1)
    # - codebook vector (normalized variable values that best characterize each 
    #   archetype) is part of the som output; the table was transposed and fig. 
    #   A1 created in R (both the table and r code attached)
    
    # plot codebook vectors of individual SOM units; RGB color definition - new color palette;includes spaces; includes axes breaks
    #somunits <- read.csv("som_4_3_hexagonal_1000000reg_soil_hanpp_yields_sort_transpose.csv", header = TRUE, sep=";")
    
    def.par <- par(no.readonly = TRUE) # save default for resetting
    myLog("Starting statistical plots...")
    
    # SOM.units >>> variables
    datasetsToNames <- list(
        "accessibility" = "Accessibility",
        "polstability" = "Polit. stability",
        "popdif50" = "Pop. dens. trend",
        "pop_glds00ag" = "Pop. density",
        "gcs_agr" = "Cap. stock in agr.",
        "agr_gdp" = "GDP in agr.",
        "gdp" = "GDP",
        "spec_rich" = "Spec. richness",
        "soil_orgc" = "Soil organic C",
        "ndvi_sd" = "NDVI-season.",
        "ndvi_mean" = "NDVI-mean",
        "temp_anom" = "Clim. anomalies",
        "bio21" = "Solar radiation",
        "bio15" = "Precip. seasonal.",
        "bio12" = "Precipitation",
        "bio2" = "Diurnal temp. range",
        "bio1" = "Temperature",
        "hanpp" = "HANPP",
        "tpi_agr" = "Tot. prod. index",
        "yg_rice" = "Gap rice",
        "yg_maize" = "Gap maize",
        "yg_wheat" = "Gap wheat",
        "y_rice" = "Yield rice",
        "y_maize" = "Yield maize",
        "y_wheat" = "Yield wheat",
        "totsederosion" = "Soil erosion",
        "irrigation" = "Irrigation",
        "nfert" = "N fertilizer",
        "grassdif50" = "Past. area trend",
        "grass2005" = "Past. area",
        "cropdif50" = "Crop. area trend",
        "crop2005" = "Crop. area")
    
    datasetsToOrder = list(
        "accessibility" = 32,
        "polstability" = 31,
        "popdif50" = 30,
        "pop_glds00ag" = 29,
        "gcs_agr" = 28,
        "agr_gdp" = 27,
        "gdp" = 26,
        "spec_rich" = 25,
        "soil_orgc" = 24,
        "ndvi_sd" = 23,
        "ndvi_mean" = 22,
        "temp_anom" = 21,
        "bio21" = 20,
        "bio15" = 19,
        "bio12" = 18,
        "bio2" = 17,
        "bio1" = 16,
        "hanpp" = 15,
        "tpi_agr" = 14,
        "yg_rice" = 13,
        "yg_maize" = 12,
        "yg_wheat" = 11,
        "y_rice" = 10,
        "y_maize" = 9,
        "y_wheat" = 8,
        "totsederosion" = 7,
        "irrigation" = 6,
        "nfert" = 5,
        "grassdif50" = 4,
        "grass2005" = 3,
        "cropdif50" = 2,
        "crop2005" = 1)
    
    somunits <- data$som.distance
    
    # transpose and create data frame including the ordering and mapping from the unused CSV (see above)
    somClasses <- as.data.frame(t(vectors))
    names(somClasses) <- paste("SOM", c(1:length(somClasses)), sep = "") # name for SOM class variables
    somunits <- merge(somClasses,
                       data.frame(variables = unlist(datasetsToNames),
                                  order = unlist(datasetsToOrder)),
                       by = "row.names")
    myLog("Somunits summary: ", paste(capture.output(summary(somunits)), sep = "\n", collapse = "\n"))
    #somunits$variables # the names for the plot
    
    require(plyr)
    somunits <- arrange(somunits, order, decreasing = TRUE)
    myLog("Somunits head:\n", paste(capture.output(head(somunits)), sep = "\n", collapse = "\n"))
    
    autolim <- function(data) {
        ylim <- c(-ceiling(max(data)), ceiling(max(data)))
        return(ylim)
    }
    
    lsaBarplot <- function(data, name, col, names = NA, leftmar = 1) {
        # used for additional space between the bars within the barplot, starting to count at the bottom
        theSpaces <- c(rep(.4, 7), 2, rep(.4, 9), 2, rep(.4, 14))
        
        par(mar = c(3, leftmar, 1, 1))
        barplot(data, main = paste("LSA ", name), xlim = autolim(data),
                horiz = TRUE,
                names.arg = names,
                cex.names=0.9,
                col = col,
                xpd = NA,
                space = theSpaces)
    }
    
    png(filename = lsaBarplotFilename, width = width, height = height)
    
    # TODO this code is not completely flexible with respect to the number of classes/figures
    numberOfFigures <- dim(codeVectors)[[1]]
    layout(matrix(c(1:numberOfFigures), 4, 4, byrow = TRUE), widths = c(1.8,1,1))
    layout.show(numberOfFigures)
    par(las=2) # make label text perpendicular to axis
    
    lsaBarplot(somunits$SOM1, name = "1", col = rgb(51,160,44, 255, maxColorValue=255), names = paste(somunits$variables), leftmar = 9)
    lsaBarplot(somunits$SOM2, name = "2", col = rgb(178,178,178, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM3, name = "3", col = rgb(152,78,163, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM4, name = "4", col = rgb(251,154,153, 255, maxColorValue=255))
    
    lsaBarplot(somunits$SOM5, name = "5", col = rgb(30,30,30, 255, maxColorValue=255), names = paste(somunits$variables), leftmar = 9)
    lsaBarplot(somunits$SOM6, name = "6", col = rgb(255,127,0, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM7, name = "7", col = rgb(166,86,40, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM8, name = "8", col = rgb(253,191,111, 255, maxColorValue=255))
    
    lsaBarplot(somunits$SOM9, name = "9", col = rgb(178,223,138, 255, maxColorValue=255), names = paste(somunits$variables), leftmar = 9)
    lsaBarplot(somunits$SOM10, name = "10", col = rgb(227,26,28, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM11, name = "11", col = rgb(55,126,184, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM12, name = "12", col = rgb(255,255,153, 255, maxColorValue=255))
    
    lsaBarplot(somunits$SOM13, name = "13", col = rgb(255,155,253, 255, maxColorValue=255), names = paste(somunits$variables), leftmar = 9)
    lsaBarplot(somunits$SOM14, name = "14", col = rgb(205,255,53, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM15, name = "15", col = rgb(255,205,153, 255, maxColorValue=255))
    lsaBarplot(somunits$SOM16, name = "16", col = rgb(155,255,100, 255, maxColorValue=255))
    
    par(def.par)  #- reset to default
    dev.off()
    
    myLog("Done with statistics plots, saved to ", lsaBarplotFilename)
}



# - calculate mean and sd of each variable for each archetype in actual 
#   variable units (Zonal statistics in ArcGIS); this was calculated from the 
#   grid layer but for simplicity it could be calculated just from the samples
#   (shapefile); Fig. 3 created in R (table and R code attached)


