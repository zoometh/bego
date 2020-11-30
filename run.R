# run different variable
# to create leaflet .html ouputs
source("func/functions_1.R") # load/run ? function

# TODO: polygons <- c("secteurs","zones","groupes")
# site
f.lflt.Bego(getwd()) # create leaflet html map of the CdT
# zones
for(i in 1:23){
  # i <- 12
  f.lflt.Z(getwd(),i)
  print(paste("write: zone",i))
}
## roches
# spatial
f.lflt.aRoche(getwd(),7,1,8) # create leaflet html map of the CdT
f.lflt.aRoche(getwd(),4,3,"16D") # create leaflet html map of a rock
# ico

figures.count(getwd())
