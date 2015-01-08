################################################################################
#
# System Archetypes Analysis Script for WPS4R
#
# Steps:
#   1) prepare input data for further analysis in R
#   2) cleaning (no NAs) and standardizing data
#   3) use self-organzing maps for land use classification
#
# Script authors: sven.lautenbach@ufz.de, tomas.vaclavik@ufz.de
# WPS editor: d.nuest@52north.org
#
################################################################################

################################################################################
# WPS4R metadata and resources
# wps.des: id = glues.systemarchetypes, title = SOM for 
# system archetypes,
# abstract = "process loads input files, then preprocesses them and creates a classification based on a self-organizing map algorithm";

# wps.resource: Input_Data_v2;

# wps.metadata: title = GLUES story page, href = http://geoportal.glues.geo.tu-dresden.de/stories/landsystemarchetypes.html;
# wps.metadata: title = Journal paper: Mapping global land system archetypes, href = http://www.sciencedirect.com/science/article/pii/S0959378013001532;
# wps.metadata: title = Datasets in catalogue, href = http://catalog.glues.geo.tu-dresden.de:8080/terraCatalog/OpenSearch.do?search=land+system+archetype&type=/Query/OpenSearch.do;

################################################################################
# required packages
require(sp)
require(raster)
require(kohonen)
require(vegan)
require(maptools)

################################################################################
# functions
myLog <- function(...) {
    cat(paste0("[glues.systemarchetypes] ", Sys.time(), " | ", ..., "\n"))
}

plot.kohcodes.my <-
    function (x, main, palette.name, bgcol=NULL, whatmap, codeRendering, 
              keepMargins, maxlegendcols, ...) 
    {
        if (!keepMargins) {
            opar <- par(c("mar", "ask"))
            on.exit(par(opar))
        }
        if (is.null(palette.name)) 
            palette.name <- terrain.colors
        whatmap <- check.whatmap(x, whatmap)
        nmaps <- length(whatmap)
        if (is.list(x$codes)) {
            if (prod(par("mfrow")) < nmaps) 
                par(ask = TRUE)
            for (i in 1:nmaps) {
                huhn <- x
                huhn$codes <- huhn$codes[[whatmap[i]]]
                if (length(main) == length(x$codes)) {
                    main.title <- main[whatmap[i]]
                }
                else {
                    if (length(main) == nmaps) {
                        main.title <- main[i]
                    }
                    else {
                        if (length(main) == 1) {
                            main.title <- main
                        }
                        else {
                            if (is.null(main)) {
                                if (!is.null(names(x$codes))) {
                                    main.title <- names(x$codes)[whatmap[i]]
                                }
                                else {
                                    main.title <- "Codes plot"
                                }
                            }
                        }
                    }
                }
                if (length(codeRendering) == length(x$codes)) {
                    cR <- codeRendering[whatmap[i]]
                }
                else {
                    if (length(codeRendering) == nmaps) {
                        cR <- codeRendering[i]
                    }
                    else {
                        cR <- codeRendering
                    }
                }
                plot.kohcodes(huhn, main = main.title, palette.name = palette.name, 
                              bgcol = bgcol, whatmap = NULL, codeRendering = cR, 
                              keepMargins = TRUE, ...)
            }
        }
        else {
            codes <- x$codes
            nvars <- ncol(codes)
            if (is.null(codeRendering)) {
                if (nvars < 15) {
                    codeRendering <- "segments"
                    maxlegendcols <- 3
                }
                else {
                    codeRendering <- "lines"
                }
            }
            margins <- rep(0.6, 4)
            if (!is.null(main)) 
                margins[3] <- margins[3] + 2
            par(mar = margins)
            if (codeRendering == "segments" & nvars < 40 & !is.null(colnames(codes))) {
                plot(x$grid, ylim = c(max(x$grid$pts[, 2]) + min(x$grid$pts[, 
                                                                            2]), -2))
                
                current.plot <- par("mfg")
                plot.width <- diff(par("usr")[1:2])
                cex <- 1
                leg.result <- legend(x = mean(x$grid$pts[, 1]), xjust = 0.5, 
                                     y = 0, yjust = 1, legend = colnames(codes), cex = cex, 
                                     plot = FALSE, ncol = min(maxlegendcols, nvars), 
                                     fill = palette.name(nvars))
                while (leg.result$rect$w > plot.width) {
                    cex <- cex * 0.9
                    leg.result <- legend(x = mean(x$grid$pts[, 1]), 
                                         xjust = 0.5, y = 0, yjust = 1, legend = colnames(codes), 
                                         cex = cex, plot = FALSE, ncol = min(maxlegendcols, 
                                                                             nvars), fill = palette.name(nvars))
                }
                leg.result <- legend(x = mean(x$grid$pts[, 1]), xjust = 0.5, 
                                     y = 0, yjust = 1, cex = cex, legend = colnames(codes), 
                                     plot = FALSE, ncol = min(maxlegendcols, nvars), 
                                     fill = palette.name(nvars), ...)
                par(mfg = current.plot)
                plot(x$grid, ylim = c(max(x$grid$pts[, 2]) + min(x$grid$pts[, 
                                                                            2]), -leg.result$rect$h))
                legend(x = mean(x$grid$pts[, 1]), xjust = 0.5, y = 0, 
                       yjust = 1, cex = cex, plot = TRUE, legend = colnames(codes), 
                       ncol = min(maxlegendcols, nvars), fill = palette.name(nvars), 
                       ...)
            }
            else {
                plot(x$grid, ...)
            }
            title.y <- max(x$grid$pts[, 2]) + 1.2
            if (title.y > par("usr")[4] - 0.2) {
                title(main)
            }
            else {
                text(mean(range(x$grid$pts[, 1])), title.y, main, 
                     adj = 0.5, cex = par("cex.main"), font = par("font.main"))
            }
            if (is.null(bgcol)) 
                bgcol <- "transparent"
            symbols(x$grid$pts[, 1], x$grid$pts[, 2], circles = rep(0.5, 
                                                                    nrow(x$grid$pts)), inches = FALSE, add = TRUE, bg = bgcol)
            if (codeRendering == "lines") {
                yrange <- range(codes)
                codes <- codes - mean(yrange)
            }
            else {
                codemins <- apply(codes, 2, min)
                codes <- sweep(codes, 2, codemins)
            }
            switch(codeRendering, segments = {
                stars(codes, locations = x$grid$pts, labels = NULL, 
                      len = 0.4, add = TRUE, col.segments = palette.name(nvars), 
                      draw.segments = TRUE)
            }, lines = {
                for (i in 1:nrow(x$grid$pts)) {
                    if (yrange[1] < 0 & yrange[2] > 0) {
                        lines(seq(x$grid$pts[i, 1] - 0.4, x$grid$pts[i, 
                                                                     1] + 0.4, length = 2), rep(x$grid$pts[i, 
                                                                                                           2], 2), col = "gray")
                    }
                    lines(seq(x$grid$pts[i, 1] - 0.4, x$grid$pts[i, 
                                                                 1] + 0.4, length = ncol(codes)), x$grid$pts[i, 
                                                                                                             2] + codes[i, ] * 0.8/diff(yrange), col = "red")
                }
            }, stars = stars(codes, locations = x$grid$pts, labels = NULL, 
                             len = 0.4, add = TRUE))
        }
        invisible()
    }

################################################################################
# color palettes, suggested by Leo Lopes
coolBlueHotRed <- function(n, alpha = 1) {
    rainbow(n, end=4/6, alpha=alpha)[n:1]
}
repRainbow <- function(n, length.col=6) {
    n.compl <- n %/% length.col
    n.rest <-  n %% length.col
    col.vec <- numeric(0)
    if (n.compl > 0)
        col.vec <- rep(rainbow(length.col), n.compl)
    if (n.rest > 0)
        col.vec <- c(col.vec, rainbow(n.compl, end= n.rest/ length.col ))
    
    return(col.vec)
}



################################################################################
# use this for testing
#wps.off;
#setwd(tempdir())
setwd("C:\\Users\\Daniel\\Documents\\2014_GLUES\\WPS4R\\System-Archetypes")
#wps.on;



################################################################################
# 1) Preprocessing
################################################################################

myLog("#### Start preprocessing (1/3) ... ")
myLog("Workspace: ", getwd())

# wps.in: id = dataPath, type = string,
# title = the path to the data layers, minOccurs = 0, maxOccurs = 1,
# abstract = "the path to the data layers, must be a local server path",
# value = Input_Data_v2;
#wps.off;
dataPath <- "Input_Data_v2"
#wps.on;

# the order of the files matters, since the conversion to SpatialPixelsDataFrame
# to do the sampling does not work for all raster layers...
files <- c(#"crop2005",
           #"cropdif50",
           #"grass2005",
           #"grassdif50",
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
           "cropdif50",
           "crop2005",
           "grass2005",
           "grassdif50",
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
sampleSize <- 1*10^6 # 100
samplingType <- "regular"
#wps.on;

myLog("Sample size: ", sampleSize, " | sampling type: ", samplingType)

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

sampleAndCombineData <- function(inputFiles, size, type) {
    myLog("Loading data from ", length(inputFiles), " datasets")
    
    rasterList <- list()
    
    for (currentFile in inputFiles) {
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
            sampelPixels <- spsample(spdf, n = size, type = type)
            samplePoints <- as(sampelPixels, "SpatialPoints")
            theDF <- cbind(coordinates(samplePoints))
        }
        
        rasterSampled <- extract(raster, samplePoints) 
        theDF <- cbind(theDF, rasterSampled)
    }
    
    # convert to data.frame
    theDF <- as.data.frame(theDF)
    names(theDF) <- c("x", "y", files)
    myLog("Created data frame with size ", toString(dim(theDF)), " and variables ",
          toString(names(theDF)))
    
    return(theDF)
}

# do the sampling
data <- sampleAndCombineData(files, sampleSize, samplingType)

# save statistics and data to files
sampleBaseDataStatistics <- "sample-data_statistics.txt"
capture.output(summary(data), file = sampleBaseDataStatistics)
# wps.out: sampleBaseDataStatistics, type = text, 
# title = statistics of dataset,
# abstract = the output of a summary operation on the created data.frame;
myLog("Saved summary statistics in file ", sampleBaseDataStatistics)
sampledBaseData <- paste0("inputData_", as.integer(sampleSize), "_",
                          samplingType, ".Rdata")
save(data, file = sampledBaseData)
# wps.out: sampledBaseData, type = rdata, title = sampled datasets,
# abstract = an R data.frame with the sampled input data for further analysis;
myLog("Saved sampled data in file ", sampledBaseData, " in directory ", getwd(),
      " | file size: ",	file.info(sampledBaseData)$size / (1024*1024), " MB")

myLog("#### Done with preprocessing (1/3)!")



################################################################################
# 2) Cleaning and standardization
################################################################################

myLog("#### Start standardization (2/3) ... ")
myLog("Workspace content: ", toString(ls()))
myLog("Input data size for standardization (# of observations, # of variables): ", toString(dim(data)))

# remove NAs first
notNA <- complete.cases(data)
#sum(nona)
data.clean <- data[notNA,]
myLog("Cleaned data size (used complete.cases to remove NAs): ",
			toString(dim(data.clean)))

# save cleaned data
cleanedDataFile <- paste0("data_cleaned.Rdata")
save(data.clean, file = cleanedDataFile)
myLog("Saved cleaned data in file ", cleanedDataFile, " in directory ", getwd(),
			" | file size: ",	file.info(cleanedDataFile)$size / (1024*1024)," MB")


# normalise the input data (not the coordinates which will not be used in the cluster analysis)

# wps.in: id = standardizationMethod, type = string,
# title = standardization method, minOccurs = 0, maxOccurs = 1,
# abstract = method for the function decostand from the package vegan,
# value = standardize;
#wps.off;
standardizationMethod <- "standardize"
#wps.on;

myLog("Normalizing input data (decostand) ... ")
data.norm <- decostand(data.clean[,-c(1:2)], standardizationMethod)
#dim(data.norm)
myLog("Done with decostand.")

# sort the variable
data.norm <- data.norm[c("crop2005", "cropdif50", "grass2005", "grassdif50", 
                         "nfert", "irrigation", "totsederosion", 
                         "y_wheat", "y_maize", "y_rice", "yg_wheat", "yg_maize", "yg_rice", 
                         "tpi_agr", "hanpp", "bio1", "bio2", "bio12", "bio15", "bio21", "temp_anom", 
                         "ndvi_mean", "ndvi_sd", "soil_orgc", "spec_rich", 
                         "gdp", "agr_gdp", "gcs_agr", "pop_glds00ag", "popdif50", 
                         "polstability", "accessibility")]
myLog("Variables (sorted): ", toString(names(data.norm)))

output.normalizedDataSummary <- "normalized-data_statistics.txt"
capture.output(summary(data.norm), file = output.normalizedDataSummary)
# wps.out: output.normalizedDataSummary, type = text, 
# title = statistics of dataset,
# abstract = the output of a summary operation on the normalized data.frame;
myLog("Saved summary statistics of normalized data in file ",
      output.normalizedDataSummary, " in ", getwd())

output.normalizedInputData <- paste0("data_normalized.Rdata")
save(data.norm, file = output.normalizedInputData)
# wps.out: output.normalizedInputData, type = rdata, title = normalized input datasets,
# abstract = an R data.frame with the input data with normalized variables which are 
# based on the cleaned input data;
myLog("Saved normalized data in file ", output.normalizedInputData, " in directory ",
			getwd(), " | file size: ",
			file.info(output.normalizedInputData)$size / (1024*1024), " MB")



################################################################################
# 3) Self-organizing map
################################################################################

myLog("#### Start som (3/3) ... ")
myLog("Workspace content: ", toString(ls()))

# wps.in: id = somGridTopology, type = string,
# title = grid topology for som, abstract = the topology of the self-organizing 
# map which can be either 'rectangular' or 'hexagonal', 
# minOccurs = 0, maxOccurs = 1, value = hexagonal;
#wps.off;
somGridTopology <- "hexagonal"
#wps.on;

# wps.in: id = somGridDim, type = string,
# title = "grid dimenstions for som in the format 'xdim,ydim'",
# abstract = "the grid dimensions for the self-organizing map in a comma 
# separated format: 'xdim,ydim' This detemines the number of classes in the SOM!",
# minOccurs = 0, maxOccurs = 1, value = "3,4";
#wps.off;
somGridDim <- "3,4"
#wps.on;

xdim <- unlist(strsplit(x = somGridDim, split = ","))[[1]]
ydim <- unlist(strsplit(x = somGridDim, split = ","))[[2]]
#topology <- list(xdim = xdim, ydim = ydim, topo = somGridTopology)
topologies <- list(list(xdim = xdim, ydim = ydim, topo = somGridTopology))

#topologies <- list(list(xdim = 3, ydim = 4, topo = somGridTopology),
#									 list(xdim = 4, ydim = 4, topo = somGridTopology),
#									 list(xdim = 5, ydim = 5, topo = somGridTopology))
myLog("Topologies: ", toString(topologies))

outputFilePrefix <- "sysarch-som_"

# wps.in: id = somIterations, type = integer,
# title = the number of loops calculating the SOM,
# abstract = "how often is the SOM analysis repeated?",
# value = 1, minOccurs = 0, maxOccurs = 1;
#wps.off;
somIterations <- 1 # 5
#wps.on;


createPDF <- function(docName, data, som, somInFeatureSpace) {
    myLog("Creating plots and PDF ...")
    
    pdf(file = docName)
    
    boxplot(data.norm)
    
    plot(som, type="changes")
    plot(som, type="codes", main="Codes")
    #add.cluster.boundaries(som, som.hc)
    plot.kohcodes.my(som,  main = "Codes", codeRendering = "segments",
                     keepMargins = FALSE, palette.name = repRainbow,
                     whatmap = NULL, maxlegendcols = 3)
    
    plot(som, type = "counts", main = "Counts")
    #add.cluster.boundaries(som, som.hc)
    plot(som, type = "quality", main = "Quality")
    #add.cluster.boundaries(som, som.hc)
    plot(som, type = "dist.neighbours", main = "SOM neighbour distances")
    #add.cluster.boundaries(som, som.hc)
    
    #####
    # plot world map with classification
    # define colors for figures
    .nunits <- length(unique(som.result$unit.classif))
    .grp.colors <- rainbow(n = .nunits)
    
    plot(somInFeatureSpace[, 1:2], col = .grp.colors[somInFeatureSpace$som.unit], 
         bg = .grp.colors[somInFeatureSpace$som.unit], pch = 16, cex = .4 )
    legend("bottomleft", paste("SOM unit", 1:.nunits),
           col = .grp.colors[1:.nunits],
           pt.bg = .grp.colors[1:.nunits], pt.cex = 1, bty = "o", pch = 16,
           bg = "white")
    
    dev.off()
    myLog("Saved plots to file ", docName, " in ", getwd())
}

createShapefile <- function(data, filename) {
    # export shapefile
    coordinates(data) <-  ~x+y
    writePointsShape(x = data, fn = paste0(filename, ".shp"), factor2char = TRUE, 
                     max_nchar=254)
    myLog("Saved shapefile ", filename, " in ", getwd())
    myLog("Shapefiles size for\t\t", list.files(pattern = filename),
          " is ",    file.info(list.files(pattern = filename))$size / (1024*1024),
          " MB")
}

# NOTE: only the last created files (last run loop) will be returned from a WPS process
for(topology in topologies) {
    myLog("Running som with topology: xdim = ", topology[[1]], ", ydim = ",
				topology[[2]], ", topo = ", topo = topology[[3]])
    .topoString <- paste0(collapse = "_", topology)
    
    for(run in 1:somIterations) {
      	myLog("Running ", run, "/", somIterations, " for ", toString(topology))
      	
      	som.grid <- somgrid(xdim = topology[[1]], ydim = topology[[2]], 
      											topo = topology[[3]])
      	myLog("som grid summary: ", toString(capture.output(summary(som.grid))))
      	
      	myLog("Starting som algorithm... current memory state: \n\tmemory.size = ",
      				memory.size(), " (max: ", memory.size(TRUE), "); memory.limit = ",
      				memory.limit())
        som.result <- som(data = as.matrix(data.norm), grid = som.grid)
      	myLog("Calculated som: ", toString(capture.output(summary(som.result))))
        #som.hc <- cutree(hclust(dist(som.result$codes)), ncl)
      	
      	myLog("SOM output created ", length(unique(som.result$unit.classif)), " classes.")
        
        # map the clusters back to the feature space
        # QUESTION: Why are we not using the normalized data for the reverse mapping?
        systemArchetypesData <- data.clean
        systemArchetypesData$som.unit <- som.result$unit.classif # save the classified values
        # map the distances to corresponding SOM codebook vector
        systemArchetypesData$som.distance <- som.result$distances
        .sampleSize <- as.integer(dim(systemArchetypesData)[[1]])
        .sampleRunString <- paste0("_sample-", .sampleSize, "_run-", run)
        
        # create code vectors table
        # wps.out: output.codeVector, type = text/csv, title = code vectors,
        # abstract = an comma-seperated values table with the code vectors of the  
        # SOM classifications;
        codeVectors <- som.result$codes
        output.codeVector <- paste0(outputFilePrefix, "codes_", .topoString, .sampleRunString, ".csv" )
        write.table(codeVectors, file = output.codeVector, sep=";", row.names = FALSE)
      	myLog("Saved code vectors file ", output.codeVector, " in ", getwd())
        
        # create output data.frame
        # wps.out: output.data, type = rdata, title = output datasets,
        # abstract = an R data.frame with the sample input data and the calculated 
        # classifications for each cell and distance to the code vector;
        output.data <- paste0(outputFilePrefix, .topoString, .sampleRunString, ".Rdata")
        save(systemArchetypesData, codeVectors, file = output.data)
        myLog("Saved sampled data (feature space) in file ", output.data, " in directory ", getwd(),
              " | file size: ",    file.info(output.data)$size / (1024*1024), " MB")
        
      	# create pdf
        # wps.out: output.plots, type = pdf, title = ouput datasets,
        # abstract = an R data.frame with the sample input data and the calculated 
        # classifications for each cell and distance to the code vector;
        output.plots <- paste0(outputFilePrefix, .topoString, .sampleRunString, ".pdf")
        createPDF(output.plots, data, som.result, systemArchetypesData)
        
        ## create shapefile
        ## wps.out: output.shapefile, type = shp, title = ouput datasets,
        ## abstract = an R data.frame with the sample input data and the calculated 
        ## classifications for each cell and distance to the code vector;
        #output.shapefile <- paste0(outputFilePrefix, .topoString, .sampleRunString)
        #createShapefile(systemArchetypesData, output.shapefile)
    } # end run loop 
} # end topologies loop

# TODO? define multiple outputs - right now only the final loop run is accessible

myLog("Output files:\n\t\t", # output.shapefile, " (shp)\n\t\t",
      output.codeVector, " (csv code vectors)\n\t\t",
      output.plots, " (plots)\n\t\t",
      output.data, " (Rdata)")
myLog("#### Done with som (3/4)")

################################################################################
# 4) Advanced plots
################################################################################
# wps.import: advanced-plots.R;

# map process output
# wps.out: output.map, type = png, title = map of LSAs,
# abstract = a global thematic map displaying the land use system archetypes;
output.map = "lsa-map.png"

# distance/quality map output
# wps.out: output.distancemap, type = png, title = som distance map,
# abstract = a global map displaying the distance of each raster cell to the code
# vector on a logarithmic scale to estimate representativeness;
output.distancemap = "lsa-som-distancemap.png"

# create statistics plot
# wps.out: output.statistics.plot, type = png, title = system archetypes
# statistics,
# abstract = barplots of the codebook vectors displaying the combination of
# normalized variable values that best characterize each land system archetype;
output.statistics.plot = "lsa-codebook-barplots.png"

pal <- createPaletteAndLabels(dim(codeVectors)[[1]])
mapData <- preparePlotData(systemArchetypesData)
saveMaps(plotData = mapData, paletteAndLabels = pal, lsaMapName = output.map,
         distanceMapName = output.distancemap)
savePlots(vectors = codeVectors, data = systemArchetypesData, paletteAndLabels = pal,
          lsaBarplotFilename = output.statistics.plot)


myLog("#### Done with plots (4/4)")
