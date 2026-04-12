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
f.lflt.aRoche(getwd(),7,1,4) # create leaflet html map
f.lflt.aRoche(getwd(),7,1,17) # create leaflet html map 
f.lflt.aRoche(getwd(),4,3,"16D") # create leaflet html map of a rock
f.lflt.aRoche(getwd(),12,5,"1@a") # create leaflet html map of a rock
f.lflt.aRoche(getwd(),11,0,"1") # create leaflet html map of a rock
# ico

figures.count(getwd())
