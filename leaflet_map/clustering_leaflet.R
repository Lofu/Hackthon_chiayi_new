library(leaflet)
library(rgdal)
library(maptools)
library(dplyr)
# 讀取csv檔
clustering <- read.csv("~/R/chiayihackthon/data/clustering.csv")
clustering <- clustering %>% mutate(county=substr(city,1,3),
                      city=substring(city,4))
list.files('~/R/chiayihackthon/leaflet_map/TOWN_MOI_1060525', pattern='\\.shp$')
file.exists('~/R/chiayihackthon/leaflet_map/TOWN_MOI_1060525/TOWN_MOI_1060525.shp')
# 底圖設定，這邊是縣市而已，要鄉鎮市的要自行去開放資料平台下載
tw <- readOGR(dsn =path.expand('~/R/chiayihackthon/leaflet_map/TOWN_MOI_1060525') ,
              layer='TOWN_MOI_1060525', stringsAsFactors = F,
              verbose = F,encoding = "utf8")
# Windows系統編碼問題
tw$COUNTYNAME <- iconv(tw$COUNTYNAME,from ="utf8" ,to="big5")
tw$TOWNNAME <- iconv(tw$TOWNNAME,from ="utf8" ,to="big5")
# 我要抓出底圖裡面的縣市，為了要跟我的資料的縣市合併，
# 因為leaflet畫圖是以底圖的縣市順序來上資料及上色
county <- tw$COUNTYNAME[which(tw$COUNTYNAME %in% c("嘉義縣","嘉義市"))]
county <- as.data.frame(county)
county <- county %>% mutate(city=tw$TOWNNAME[which(tw$COUNTYNAME %in% c("嘉義縣","嘉義市"))])
# 跟我的資料合併，ttt放左邊左欄left_join合併過來
clustering <- left_join(county,clustering)
#調整顏色
col.raw <-  c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F"
              ,"#FF7F00","#CAB2D6","#6A3D9A") 
pal <- colorFactor(
  palette = c(col.raw[1],col.raw[2],col.raw[3],col.raw[4],col.raw[5]),
  domain = clustering$clustering
)

# 開始畫地圖囉~~這邊有些我也沒有很懂，就是套用XD
leaflet(
  tw %>% subset(tw$COUNTYNAME %in% clustering$county & tw$TOWNNAME %in% clustering$city)) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    # popup是對話框的設定
    popup = ~sprintf("<strong><span style=\"color:gray;\"> 縣市 </span></strong></br>
                     <span style=\"font-size:20px;\"> %s </span></br>
                     <strong><span style=\"color:gray;\"> 鄉鎮 </span></strong></br>
                     <span style=\"font-size:20px;\"> %s </span></br>
                     <strong><span style=\"color:gray;\"> 群 </span></strong></br>
                     <span style=\"font-size:40px;\"> %s",COUNTYNAME, TOWNNAME ,clustering$clustering ),
    color = ~pal(clustering$clustering)) %>%
  addLegend("bottomright", pal = pal, values = ~clustering$clustering,
            title = "群別",
            opacity = 0.5) %>% #透明度
  # 這個是初始畫面，定位的位置
  setView(lng = 120.5621971, lat = 23.35, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron")
#  addTiles()
# addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', 
#          attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> 
#                         &copy; <a href="http://cartodb.com/attributions">CartoDB</a>') 