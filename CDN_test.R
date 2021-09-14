# bash 
# Rscript --vanilla -e 'source("CDN_test.R", echo=TRUE)' > CDN_test.Rout 2>&1

td <- tempfile()
dir.create(td)
Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)
library(rgdal)
b_pump <- readOGR(system.file("vectors/b_pump.gpkg", package="rgdal"))
WKT <- wkt(b_pump)
cat(WKT, "\n")
if (is.projected(b_pump)) { 
  o <- project(t(unclass(bbox(b_pump))), wkt(b_pump), inv=TRUE)
} else {
  o <- t(unclass(bbox(b_pump)))
}

(aoi <- c(t(o + c(-0.1, +0.1))))
is_proj_CDN_enabled()
enable_proj_CDN()
is_proj_CDN_enabled()
(shpr <- get_proj_search_paths())
shpr[1]
try(file.size(file.path(shpr[1], "cache.db")))
list_coordOps(WKT, "OGC:CRS84", area_of_interest=aoi)
is1m <- spTransform(b_pump, CRS(SRS_string="OGC:CRS84"))
get_last_coordOp()
try(file.size(file.path(shpr[1], "cache.db")))
library(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(shpr[1], "cache.db"))
dbListTables(db)
dbReadTable(db, "chunks")
dbDisconnect(db)
disable_proj_CDN()
is_proj_CDN_enabled()
sessionInfo()
