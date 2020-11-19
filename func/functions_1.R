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
tab.latin <- data.frame(arabe=c(1:12),
                        latin=c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII"),
                        stringsAsFactors = F)

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
  con <- conn.pg()
  sqll <- "select zone,groupe,roche,nom,histoseule,geographie ,ST_X(ST_Transform(wkb_geometry, 4326)) as x ,ST_Y(ST_Transform(wkb_geometry, 4326)) as y from roches_gravees"
  roches.all <- dbGetQuery(con,sqll)
  aRoche <- roches.all[roches.all$zone == Z & roches.all$groupe == G & roches.all$roche == R,]
  # transcript
  # Z <- 7 ; G <- 1 ; R <- 8
  aZ <- tab.latin[tab.latin$arabe == Z,"latin"]
  aG <- tab.latin[tab.latin$arabe == G,"latin"]
  aR <- as.character(R)
  aRocheName <- paste0("Z",aZ,"G",aG,"R",aR)
  aPath <- paste0("Bego/Images/Images Faces/Z",aZ,"/Z",aZ,"G",aG,"/Z",aZ,"G",aG,"R",aR,".gif")
  aImg <- drop_media(aPath)
  # stele.chef.lk <- drop_media("Bego/Images/Images Faces/ZVII/ZVIIGI/ZVIIGIR8.gif")
  Plan_lk <- paste0('<img src="',
                    # stele.chef.lk,
                    aImg,
                    '" style="width: 55vw; max-width: 200px;" >')
  dbDisconnect(con)
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
  saveWidget(aRoche, file=paste0(chm,"/img/",aRocheName,".html"))
}

f.lflt.Bego <- function(chm){
  # create leaflet obj for the Bego zones
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