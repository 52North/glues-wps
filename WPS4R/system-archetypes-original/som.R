################################################################################
#
# som.r
#
# purpose: use self-organuzing maps on syndromes 
#
# author: sven.lautenbach@ufz.de, tomas.vaclavik@ufz.de
#
#
# overview about the analysis
# 1) standardize data
# 2) som
#
################################################################################

################################################################################
# log function
myLog <- function(...) {
	cat(paste0("[glues.systemarchetypes] ", Sys.time(), " | ", ..., "\n"))
}
myLog("Start script SOM ... ")

################################################################################
# helper functions and palettes
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

# color palettes
## palette suggested by Leo Lopes
coolBlueHotRed <- function(n, alpha = 1) {
	rainbow(n, end=4/6, alpha=alpha)[n:1]
}
# 
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
# load input data and required packages
require(kohonen)
require(vegan)
require(sp)
require(maptools)

# wps.des: id = glues.systemarchetypes.som, title = som analysis for 
# system archetypes, abstract = self-organizing map based on preprocessed input 
# data;

#wps.off;
setwd(tempdir())
#wps.on;

myLog("Working directory: ", getwd())

# wps.in: id = dataPath, type = string,
# title = the path to the data layers, minOccurs = 0, maxOccurs = 1,
# value = D:/Dokumente/2014_GLUES/WPS4R/System-Archetypes/Input_Data_v2/;
#wps.off;
dataPath <- "D:/Dokumente/2014_GLUES/WPS4R/System-Archetypes/"
#wps.on;

# -> theDF, dataframe with the sample points
# wps.in: id = fileName, type = string,
# title = the path to the data layers, minOccurs = 0, maxOccurs = 1,
# value = inputData_1000000_regular.Rdata;
#wps.off;
fileName <- "inputData_1000000_regular.Rdata"
#wps.on;

# TODO if the input file is not existing, then run the wps_preprocessing script?

filePath <- paste0(dataPath, fileName)
myLog("Loading file from path ", filePath, " (exists: ", file.exists(filePath), ")")
load(file = filePath)
myLog("Loaded file, workspace content: ", toString(ls()))

# wps.in: id = dataVarName, type = string,
# title = the path to the data layers, minOccurs = 0, maxOccurs = 1,
# value = inputData_1000000_regular.Rdata;
#wps.off;
dataVarName <- "theDF"
#wps.on;

data <- get(dataVarName)
myLog("Input data size: ", toString(dim(data)))

# remove NAs first
notNA <- complete.cases(data)
#sum(nona)
data.clean <- data[notNA,]
myLog("Cleaned data size (used complete.cases to remove NAs): ",
			toString(dim(data.clean)))

somData <- paste0("som_data.Rdata")
save(data.clean, file = somData)
# wps.out: somData, type = rdata, title = output datasets,
# abstract = an R data.frame with (i) the input data (all NA values removed), 
# (ii) the classification based on an SOM algorithm, and (iii) the code vectors 
# mapped back to feature space;
myLog("Saved output data in file ", somData, " in directory ", getwd(),
			" | file size: ",	file.info(somData)$size / (1024*1024)," MB")


################################################################################
# normalise the input data (not the coordinates which will not be used in the cluster analysis)

# wps.in: id = standardizationMethod, type = string,
# title = standardization method, minOccurs = 0, maxOccurs = 1,
# abstract = method for the function decostand from the package vegan,
# value = standardize;
#wps.off;
standardizationMethod <- "standardize"
#wps.on;

myLog("Normalizing input data... ")
data.norm <- decostand(data.clean[,-c(1:2)], standardizationMethod)
#dim(data.norm)

normalizedDataSummary <- "normalized-data_statistics.txt"
capture.output(summary(data.norm), file = normalizedDataSummary)
# wps.out: normalizedDataSummary, type = text, 
# title = statistics of dataset,
# abstract = the output of a summary operation on the normalized data.frame;
myLog("Saved summary statistics of normalized data in file ",
			normalizedDataSummary, " in ", getwd())

# sort the variable
data.norm <- data.norm[c("crop2005", "cropdif50", "grass2005", "grassdif50", "nfert", "irrigation", "totsederosion", "y_wheat", "y_maize", "y_rice", "yg_wheat", "yg_maize", "yg_rice", "tpi_agr", "hanpp", "bio1", "bio2", "bio12", "bio15", "bio21", "temp_anom", "ndvi_mean", "ndvi_sd", "soil_orgc", "spec_rich", "gdp", "agr_gdp", "gcs_agr", "pop_glds00ag", "popdif50", "polstability", "accessibility")]
myLog("Variables (sorted): ", toString(names(data.norm)))

normalizedInputData <- paste0("som_data_normalized.Rdata")
save(data.norm, file = normalizedInputData)
# wps.out: normalizedInputData, type = rdata, title = normalized input datasets,
# abstract = an R data.frame with the input data with normalized variables, 
# based on the cleaned input data;
myLog("Saved output data in file ", normalizedInputData, " in directory ",
			getwd(), " | file size: ",	file.info(normalizedInputData)$size / (1024*1024),
			" MB")

################################################################################
# SOM

# wps.in: id = somGridTopology, type = string,
# title = grid topology for som, minOccurs = 0, maxOccurs = 1,
# value = hexagonal;
#wps.off;
somGridTopology <- "hexagonal"
#wps.on;

# wps.in: id = somGridDim, type = string,
# title = "grid dimenstions for som in the format 'xdim,ydim'",
# minOccurs = 0, maxOccurs = 1, value = 3,4;
#wps.off;
somGridDim <- "4,4"
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
iterations <- 1 # 5

# QUESTION: Why are we actually running this several times? The variable "run"
# is only used in filenames, not in analysis...
for(topology in topologies) {
	myLog("Running som with topology: xdim = ", topology[[1]], ", ydim = ",
				topology[[2]], ", topo = ", topo = topology[[3]])
  for(run in 1:iterations) {
  	myLog("Running ", run, "/", iterations, " for ", toString(topology))
  	
  	som.grid <- somgrid(xdim = topology[[1]], ydim = topology[[2]], 
  											topo = topology[[3]])
  	myLog("Grid summary: ", toString(capture.output(summary(som.grid))))
  	
  	myLog("Starting som algorithm...")
    syndromes.som <- som(data= as.matrix(data.norm), grid = som.grid)
  	myLog("Calculated som: ", toString(capture.output(summary(syndromes.som))))
    #som.hc <- cutree(hclust(dist(syndromes.som$codes)), ncl)
  	
  	nunits <- length(unique(syndromes.som$unit.classif))
  	myLog("SOM output created ", nunits, " classes.")
    
    # save code vectors to table
  	fncsv <- paste0(outputFilePrefix, "codes_", paste0(collapse="_", topology), "_1000000reg", run, ".csv" )
    write.table(syndromes.som$codes, file = fncsv, sep=";", row.names = FALSE)
  	myLog("Saved code vectors file ", fncsv, " in ", getwd())
    
  	# create pdf
  	myLog("Creating plots...")
  	fnpdf <- paste0("som_",  paste0(collapse="_", topology), "_1000000reg", run, ".pdf")
    pdf(file = fnpdf)
  	
    boxplot(data.norm)
  	
    plot(syndromes.som, type="changes")
    plot(syndromes.som, type="codes", main="Codes")
    #add.cluster.boundaries(syndromes.som, som.hc)
    plot.kohcodes.my(syndromes.som,  main = "Codes", codeRendering = "segments",
    								 keepMargins = FALSE, palette.name = repRainbow,
    								 whatmap = NULL, maxlegendcols = 3)
    
    plot(syndromes.som, type = "counts", main = "Counts")
    #add.cluster.boundaries(syndromes.som, som.hc)
    plot(syndromes.som, type = "quality", main = "Quality")
    #add.cluster.boundaries(syndromes.som, som.hc)
    plot(syndromes.som, type = "dist.neighbours", main = "SOM neighbour distances")
    #add.cluster.boundaries(syndromes.som, som.hc)
    
    #####
    # map the clusters back to the feature space
  	
  	# QUESTION: Why are we not using the normalized data for the reverse mapping?
  	som.data <- data.clean
  	som.data$som.unit <- syndromes.som$unit.classif
    
    # map the distances to corresponding SOM codebook vector
  	som.data$som.distance <- syndromes.som$distances
    
  	#####
  	# plot world map with classification
  	# define colors for figures
  	grp.colors <- rainbow(n = nunits)
  	
    plot(som.data[, 1:2], col = grp.colors[som.data$som.unit], 
    		 bg = grp.colors[som.data$som.unit], pch = 16, cex = .4 )
    legend("bottomleft", paste("SOM unit", 1:nunits),
    			 col = grp.colors[1:nunits],
    			 pt.bg = grp.colors[1:nunits], pt.cex = 1, bty = "o", pch = 16,
    			 bg = "white")
    
    dev.off()
  	myLog("Saved plots to file ", fnpdf, " in ", getwd())
    
    # export shapefile
    coordinates(som.data) <-  ~x+y
  	fnshp <- paste0("som_",  paste0(collapse = "_", topology), "_1000000reg", run)
    writePointsShape(x = som.data, fn = paste0(fnshp, ".shp"), factor2char = TRUE, 
    								 max_nchar=254)
  	myLog("Saved shapefile ", fnshp, " in ", getwd())
  	myLog("Shapefiles size for\t\t", list.files(pattern = fnshp),
  				" is ",	file.info(list.files(pattern = fnshp))$size / (1024*1024),
  				" MB")
  } # end run loop 
} # end topologies loop 

myLog("Done!")

# TODO define outputs... do we actually want the user to access differnt som
# configurations, or just one?
