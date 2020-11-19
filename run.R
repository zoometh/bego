# run different variable
# to create leaflet .html ouputs
source("func/functions_1.R") # load/run ? function

# TODO: polygons <- c("secteurs","zones","groupes")
f.lflt.Bego(getwd()) # create leaflet html map of the CdT
f.lflt.aRoche(getwd(),7,1,8) # create leaflet html map of the CdT
f.lflt.aRoche(getwd(),4,3,"16D") # create leaflet html map of a rock

figures.count(getwd())
