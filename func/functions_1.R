library(htmlwidgets)
library(kableExtra)
library(dplyr)
library(knitr)
library(magick)
library(leaflet)
library(RPostgreSQL)
library(rpostgis)
library(rdrop2)
library(sp)
library(plotly)


wgs84 <- CRS("+proj=longlat +datum=WGS84")
# tab.latin <- data.frame(arabe=c(1:12),
#                         latin=c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII"),
#                         stringsAsFactors = F)

conn.pg <- function(){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,
                   dbname="bego",
                   host="localhost",
                   port=5432,
                   user="postgres",
                   password="postgres")
}

reproj <- function(a.geom){
  # reproject
  a.geom <- spTransform(a.geom, wgs84)
  return(a.geom)
}

figures.count <- function(chm){
  # ceate plotly count on 'prem'
  figures <- read.table("img/FIGURES.csv",sep = ",",header = T)
  figures$zone <- as.factor(figures$zone)
  figures$prem <- toupper(figures$prem)
  # figures.prem.sum <- figures.prem.sum[!is.na(figures.prem.sum$prem),] # rm NA
  figures.prem.sum <- figures %>% count(prem, zone)
  figures.prem.sum <- figures.prem.sum[figures.prem.sum$prem != '',] # rm NA
  figures.prem.sum <- figures.prem.sum[figures.prem.sum$prem != 'N',] # rm N
  # table(zone = figures.prem.sum$zone, figures.prem.sum$prem)
  
  fig.count <- figures.prem.sum %>%
    plot_ly(x = ~zone, y = ~n, type = 'bar', 
            name = ~prem, color = ~prem) %>%
    layout(yaxis = list(title = 'Count'), barmode = 'stack')
  htmlwidgets::saveWidget(as_widget(fig.count),paste0(chm,"/img/figures_count_by_zones.html"))
}


f.lflt.aRoche <- function(chm,Z,G,R){
  # create leaflet obj for the chef de tribu and sect Me
  # add the image
  # Z <- 4 ; G <- 3 ; R <- "16D" ; R <- "1@a"
  con <- conn.pg()
  sqll <- "select zone,groupe,roche,nom,histoseule,geographie ,ST_X(ST_Transform(wkb_geometry, 4326)) as x ,ST_Y(ST_Transform(wkb_geometry, 4326)) as y from roches_gravees"
  roches.all <- dbGetQuery(con,sqll)
  aRoche <- roches.all[roches.all$zone == Z & roches.all$groupe == G & roches.all$roche == R,]
  dbDisconnect(con)
  Plan_lk <- f.ico.aRoche(Z,G,R)[[2]] # the Plan_lk
  aRoche  <- leaflet(width = "50%") %>%
    setView(lng = aRoche$x, lat = aRoche$y, zoom=18) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    # all rocks
    addCircleMarkers(lng=roches.all$x, lat=roches.all$y,
                     popup="engraved<br>rock",radius = 1,opacity = 0.3) %>%
    addCircleMarkers(lng=aRoche$x, lat=aRoche$y,
                     popup = Plan_lk,
                     color = "red",
                     radius = 2,
                     opacity = 0.7)
  R <- gsub("@","x",R)
  aRocheName <- paste0("Z",Z,"G",G,"R",R)
  saveWidget(aRoche, file=paste0(chm,"/img/",aRocheName,".html"))
}

f.lflt.Bego <- function(chm){
  # create leaflet obj for the Bego zones
  # chm <- "C:/Users/supernova/Dropbox/My PC (supernova-pc)/Documents/bego"
  con <- conn.pg()
  sqll <- "select zone,groupe,roche,nom,histoseule,geographie ,ST_X(ST_Transform(wkb_geometry, 4326)) as x ,ST_Y(ST_Transform(wkb_geometry, 4326)) as y from roches_gravees"
  roches.all <- dbGetQuery(con,sqll)
  # label
  roches.all$lbl <- paste0(roches.all$zone,".",
                           roches.all$groupe,".",
                           roches.all$roche,".")
  # load zones
  zones <- pgGetGeom(con,
                     c("public", "zones"),
                     geom = "geom",
                     gid = "gid",
                     other.cols = c("secteur"),
                     clauses  = "WHERE secteur not like ''")
  zones <- reproj(zones) # reproj
  dbDisconnect(con)
  Bego <- leaflet(width = "50%") %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    # secteurs
    addPolygons(data = zones,
                popup= ~secteur,
                stroke = TRUE,
                color = "#000000",
                weight = 2,
                fillOpacity = 0,
                smoothFactor = 0.5) %>% 
    # all rocks
    addCircleMarkers(lng=roches.all$x,
                     lat=roches.all$y,
                     popup=roches.all$lbl,
                     radius = 0.5,
                     opacity = 0.3)
  saveWidget(Bego, file=paste0(chm,"/img/Bego.html"))
}

f.lflt.Z <- function(chm, Z){
  # create leaflet obj for the Bego zones
  # chm <- "C:/Users/supernova/Dropbox/My PC (supernova-pc)/Documents/bego"
  # Z <- 12
  con <- conn.pg()
  sqll <- "select zone,groupe,roche,nom,histoseule,geographie ,ST_X(ST_Transform(wkb_geometry, 4326)) as x ,ST_Y(ST_Transform(wkb_geometry, 4326)) as y from roches_gravees"
  sqll <- paste0(sqll, " where zone =", Z)
  roches.Z <- dbGetQuery(con,sqll)
  # label
  roches.Z$lbl <- paste0(roches.Z$zone,".",
                         roches.Z$groupe,".",
                         roches.Z$roche,".")
  # load zones
  zones <- pgGetGeom(con,
                     c("public", "zones"),
                     geom = "geom",
                     gid = "gid",
                     other.cols = c("zone","secteur"),
                     clauses  = paste0("WHERE zone = ",Z," AND indice = 2"))
  zones <- reproj(zones) # reproj
  dbDisconnect(con)
  Zone <- leaflet(width = "50%") %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    # secteurs
    # setView(zoom=1, mat(me)) %>% 
    addPolygons(data = zones,
                popup= ~paste0("zone ",as.character(zone)),
                stroke = TRUE,
                color = "#000000",
                weight = 2,
                fillOpacity = 0,
                smoothFactor = 0.5) %>% 
    # all rocks
    addCircleMarkers(lng=roches.all$x,
                     lat=roches.all$y,
                     popup=roches.all$lbl,
                     radius = 0.5,
                     opacity = 0.3)
  saveWidget(Zone, file=paste0(chm,"/img/Z",Z,".html"))
}

# select zone from zones where indice = 2 -- groupe = 666 and zone <> 666
# order by zone


f.bego.plan <- function(roches.xy){
  # load plan from GDropBox
  plan.path <- "D:\\\\Base de Donnees"
  # \\\\Images\\\\Images Faces\\\\
  roches.xy$Plan <- gsub(plan.path,"Bego",roches.xy[,"Plan"])
  roches.xy$Plan <- gsub("\\\\","/",roches.xy[,"Plan"])
  roches.xy$Plan_lk <- NA
  for (i in 1:nrow(roches.xy)){
    # i <- 1
    a.img <- drop_media("Bego/Images/SS_Plan.bmp")
    try(a.img <- drop_media(roches.xy[i,"Plan"]),
        silent = T)
    # a.img <- drop_media('Sauri/R11/IMG_8279.JPG')
    roches.xy[i,"Plan_lk"] <- path.img <- paste0('<img src="',
                                                 a.img$link,
                                                 # 'https://drive.google.com/uc?export=view&id=',
                                                 # a.rock.fold,
                                                 # '1DBlLa-PnN2hWcAIoqf5jhb7rIOarIM7y',
                                                 '" style="width: 55vw; max-width: 200px;" >')
  }
  return(roches.xy)
}