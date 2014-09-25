################################################################################
#
# System Archetypes Output Testing Script for WPS4R
#
# Steps:
#   1) load pre-calculated Rdata file
#   2) create output files
#
# Script authors: sven.lautenbach@ufz.de, tomas.vaclavik@ufz.de
# WPS editor: d.nuest@52north.org
#
################################################################################

################################################################################
# WPS4R metadata and resources
# wps.des: id = glues.systemarchetypes.output-test, title = Create output files
# abstract = create all som output files based on an existing analysis run;

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
setwd("C:/Users/Daniel/Documents/2014_GLUES/WPS4R/System-Archetypes/")

# save test data from running full-process.R
#save(data.norm, output.plots, .topoString, som.result, .som.fs, .sampleRunString, .sampleSize, file = "44hexa_103.Rdata")

#wps.on;

################################################################################
# load test data

# wps.resource: 44hexa_103.Rdata;

# wps.in: id = testdata, type = string, title = file with R objects for testing output, 
# minOccurs = 0, maxOccurs = 1, value = 44hexa_103.Rdata;
#wps.off;
testdata <- "44hexa_103.Rdata"
#wps.on;

load(file = testdata, verbose = TRUE)


################################################################################
# create outputs
outputFilePrefix <- "sysarch-som_testing_"

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


# wps.out: output.codeVector, type = text/csv, title = code vectors,
# abstract = an comma-seperated values table with the code vectors of the  
# SOM classifications;
# save code vectors to table
output.codeVector <- paste0(outputFilePrefix, "codes_", .topoString, .sampleRunString, ".csv" )
write.table(som.result$codes, file = output.codeVector, sep=";", row.names = FALSE)
myLog("Saved code vectors file ", output.codeVector, " in ", getwd())

# wps.out: output.data, type = rdata, title = output datasets,
# abstract = an R data.frame with the sample input data and the calculated 
# classifications for each cell and distance to the code vector;
output.data <- paste0(outputFilePrefix, .topoString, .sampleRunString, ".Rdata")
systemArchetypesData <- .som.fs
save(systemArchetypesData, file = output.data)
myLog("Saved sampled data (feature space) in file ", output.data, " in directory ", getwd(),
      " | file size: ",	file.info(output.data)$size / (1024*1024), " MB")
rm(systemArchetypesData)

# create pdf
# wps.out: output.plots, type = pdf, title = ouput datasets,
# abstract = an R data.frame with the sample input data and the calculated 
# classifications for each cell and distance to the code vector;
output.plots <- paste0(outputFilePrefix, .topoString, .sampleRunString, ".pdf")
createPDF(output.plots, data, som.result, .som.fs)

# create shapefile
# wps.out: output.shapefile, type = shp_x, title = ouput datasets,
# abstract = an R data.frame with the sample input data and the calculated 
# classifications for each cell and distance to the code vector;
output.shapefile <- paste0(outputFilePrefix, .topoString, .sampleRunString)
createShapefile(.som.fs, output.shapefile)

myLog("Output files:\n\t\t", output.shapefile, " (shp)\n\t\t",
      output.codeVector, " (csv code vectors)\n\t\t",
      output.plots, " (plots)\n\t\t",
      output.data, " (Rdata)")

myLog("#### Done with testdata output for som")
