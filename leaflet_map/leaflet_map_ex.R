setwd("~/R/leaflet_map")
library(leaflet)
library(rgdal)
library(maptools)
# 底圖設定，這邊是縣市而已，要鄉鎮市的要自行去開放資料平台下載
tw <- readOGR(dsn = 'County_MOI_1051214/COUNTY_MOI_1051214.shp',
              layer='COUNTY_MOI_1051214', stringsAsFactors = F,
              verbose = F,encoding = "utf8")
# Windows系統編碼問題
tw$COUNTYNAME <- iconv(tw$COUNTYNAME,from ="utf8" ,to="big5")
# 我要抓出底圖裡面的縣市，為了要跟我的資料的縣市合併，
# 因為leaflet畫圖是以底圖的縣市順序來上資料及上色
ttt <- tw$COUNTYNAME
ttt <- as.data.frame(ttt)
colnames(ttt) <- "county"
# 跟我的資料合併，ttt放左邊左欄left_join合併過來
dog <- left_join(ttt,dog)
dog$count[is.na(dog$count)] <- 0
# 切range
dog$range <- cut(dog$count,c(0,100,200,300,400,500),
                 c("1-100","101-200","201-300","301-400","401-500"))
dog$range <- as.character(dog$range)
dog$range[is.na(dog$range)] <- 0

#調整顏色
pal <- colorFactor(
  palette = c("#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000"), #c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00")
  domain = dog$range
)

# 開始畫地圖囉~~這邊有些我也沒有很懂，就是套用XD
leaflet(
  tw %>% subset(tw$COUNTYNAME %in% dog$county)) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
    # popup是對話框的設定
    popup = ~sprintf("<strong><span style=\"color:gray;\"> 數量 </span></strong></br>
                     <span style=\"font-size:40px;\"> %s </span></br>
                     <strong><span style=\"color:gray;\"> 縣市 </span></strong></br>
                     <span style=\"font-size:20px;\"> %s </span>",dog$range, COUNTYNAME ),
    color = ~pal(dog$range)) %>%
  addLegend("bottomright", pal = pal, values = ~dog$range,
            title = "數量",
            opacity = 0.5) %>% #透明度
  # 這個是初始畫面，定位的位置
  setView(lng = 120.5621971, lat = 22.92, zoom = 6) %>%
  addProviderTiles("CartoDB.Positron")
#  addTiles()
# addTiles(urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', 
#          attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> 
#                         &copy; <a href="http://cartodb.com/attributions">CartoDB</a>') 