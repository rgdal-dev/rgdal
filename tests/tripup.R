suppressPackageStartupMessages(library(rgdal))
load(system.file("etc/test_dfs.RData", package="rgdal"))
load(system.file("etc/obj_with_comments.RData", package="rgdal"))
Ps1_nc <- Ps1
comment(Ps1_nc) <- NULL
comment(slot(Ps1_nc, "polygons")[[1]]) <- NULL
drivers <- c("GeoJSON", "ESRI Shapefile")
drivers <- drivers[drivers %in% ogrDrivers()$name]
tfbase <- tempfile()
for (driver in drivers) {
tf <- paste0(tfbase, driver, "P", sep=".")
writeOGR(SpatialPolygonsDataFrame(P, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("P with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(P, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(P, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(P, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "Ph", sep=".")
writeOGR(SpatialPolygonsDataFrame(Ph, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("Ph with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(Ph, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(Ph, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(Ph, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "Phs", sep=".")
writeOGR(SpatialPolygonsDataFrame(Phs, data=df9), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("Phs with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(Phs, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(Phs, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(Phs, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "MP", sep=".")
writeOGR(SpatialPolygonsDataFrame(MP, data=df3), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("MP with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(MP, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(MP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(MP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
tf <- paste0(tfbase, driver, "MPh", sep=".")
writeOGR(SpatialPolygonsDataFrame(MPh, data=df3), tf, "GeoJSON",
 driver=driver, verbose=TRUE)
#rP <- as(readOGR(tf, "GeoJSON", verbose=FALSE), "SpatialPolygons")
rP <- as(readOGR(tf, verbose=FALSE), "SpatialPolygons")
cat("MPh with driver:", driver, "\n")
unlink(paste(tf, "*", sep=""), recursive=driver == "ESRI Shapefile")
cat(" comment", isTRUE(all.equal(sapply(slot(rP, "polygons"), comment), sapply(slot(MPh, "polygons"), comment))), "\n")
cat(" coords", isTRUE(all.equal(lapply(slot(MPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "coords")), check.attributes=FALSE)), "\n")
cat(" holes", isTRUE(all.equal(lapply(slot(MPh, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), lapply(slot(rP, "polygons"), function(x) lapply(slot(x, "Polygons"), slot, "hole")), check.attributes=FALSE)), "\n")
load(system.file("etc/obj_without_comments.RData", package="rgdal"))

}
