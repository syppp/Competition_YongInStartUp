library(tidyverse,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(data.table,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(showtext,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rgdal,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(lmtest,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(gstat,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(spdep,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(RColorBrewer,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(classInt,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(spatialreg,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(car,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(leaflet,warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
font_add_google('Nanum Pen Script', 'nanumpen')
showtext_auto()

data <- fread("data_f.csv",
             header = T,
             stringsAsFactors = F,
             data.table = F,
             encoding = "UTF-8",
             drop = "Y_youth_count")

data$cluster <- as.factor(data$cluster) 

data_16 <- readOGR(dsn = "16.용인시_소상공인_매출정보.geojson")
my_shp <- data_16[which(data_16$gid %in% data$gid), ] 

my_nb <- poly2nb(my_shp, queen = FALSE)
my_listw <- nb2listw(my_nb, style = "W", zero.policy = TRUE)

moran.test(data$Y_stable, my_listw , zero.policy = TRUE)

local <- localmoran(x = data$Y_stable,listw = my_listw, zero.policy = TRUE)

quadrant <- vector(mode="numeric",length=nrow(local))
m.qualification <-data$Y_stable - mean(data$Y_stable)
m.local <- local[,1] - mean(local[,1])
signif <- 0.1
quadrant[m.qualification >0 & m.local>0] <- 4
quadrant[m.qualification <0 & m.local<0] <- 1
#quadrant[m.qualification <0 & m.local>0] <- 2
#quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0

# plot in r
brks <- c(0,1,4)
colors <- c("lightgrey","#011d82","#750a08")
plot(my_shp, border="white",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft",legend=c("insignificant","low-low","high-high"),
fill=colors,bty="n")


scale_fun <- function(X) as.vector(scale(X))

data2 <- data %>% select(-gid)
data_scaled <- data2 %>% mutate_if(is.numeric, scale_fun)
data_scaled$Y_stable <- data$Y_stable

lm.fit <- lm(log(Y_stable)~., data =data_scaled) 

summary(lm.fit)

lm.LMtests(lm.fit, my_listw, test="all", spChk=NULL, naSubset=TRUE, zero.policy = TRUE) 

lm.fit_slx = lmSLX(log(Y_stable)~ ., data =data_scaled ,listw=my_listw, zero.policy = TRUE)

summary(lm.fit_slx)

print(paste0("일반회귀모형 Adjisted R2 : " ,round(summary(lm.fit)$adj.r.squared,4)))
print(paste0("공간시차모형 Adjisted R2 : " ,round(summary(lm.fit_slx)$adj.r.squared,4)))

par(mfrow = c(2,2))
plot(lm.fit_slx)

df <- data.frame(name = names(coefficients(lm.fit_slx)),
                 value = coefficients(lm.fit_slx)) %>% 
       spread(name, value) %>% 
       select(-c(starts_with("lag"), starts_with("("))) %>% 
       gather(key = "name", value = "value")

p1 <- df %>% ggplot(aes(x = reorder(name, value), y = value)) + 
    geom_col(aes(fill = value, color = value), alpha = 0.2) + 
    coord_flip() +
    geom_text(aes(label = round(value,2)), size = 4.5, position = position_stack(0.5)) + 
    scale_fill_gradient2(mid = "white",
                         low = "#0057a3", 
                         high = "#a9cd07",
                         midpoint = 0, 
                         limit = c(min(df$value),max(df$value))) +
    scale_color_gradient2(mid = "lightgray",
                          low = "#0057a3",
                          high = "#a9cd07",
                          midpoint = 0, 
                          limit = c(min(df$value),max(df$value))) +
    theme_classic() + theme(legend.position = 'none')

p1

filtering <- data %>% filter(data$gid_variety > 5 & age_4050 > 49.20 & flpop_rate < -0.00721 & 
                             found_mid < 0.7143 & cluster == '중심상권' ) 
filtering

## 지도 시각화를 위한 맵핑(Mapping)

filtering_map <- data_16[which(data_16$gid %in% filtering$gid), ] 
filtering_map@data$id <- rownames(filtering_map@data)
map = fortify(filtering_map, region = "id")
map_merge<- merge(map, filtering_map@data, by = "id")

## 좌표 계산

map_merge %>% filter(gid == "다사62aa19ba") %>% select(long, lat)
map_merge %>% filter(gid == "다사62aa19bb") %>% select(long, lat)

leaflet(map_merge) %>% 
  setView(lng = 127.0714 , lat = 37.27590, zoom = 15.5) %>% 
  addProviderTiles('OpenStreetMap.Mapnik') %>% 
  addRectangles(
    lng1=127.0714, lat1 = 37.27364,
    lng2=127.0742, lat2 = 37.27590,
    fillColor = "#a9cd07", color ='#014700') %>%  
  addRectangles(
    lng1=127.0714, lat1 = 37.27589,
    lng2=127.0742, lat2 = 37.27816,
    fillColor = "#a9cd07", color ='#014700')  

## 좌표 계산

map_merge %>% filter(gid == "다사64aa25aa") %>% select(long, lat)

leaflet(map_merge) %>% 
  setView(lng = 127.0951, lat = 37.32443, zoom = 16.5) %>% 
  addProviderTiles('OpenStreetMap.Mapnik') %>% 
  addRectangles(
    lng1=127.0937, lat1=37.32330,
    lng2=127.0965, lat2=37.32556,
    fillColor = "#a9cd07", color ='#014700') #다사64aa25aa

## 좌표 계산

map_merge %>% filter(gid == "다사66ab21ba") %>% select(long, lat)

leaflet(map_merge) %>% 
  setView(lng =127.1206, lat=37.29296, zoom = 16.6) %>% 
  addProviderTiles('OpenStreetMap.Mapnik') %>% 
  addRectangles(
    lng1=127.1192, lat1 = 37.29183,
    lng2=127.1220, lat2 = 37.29409,
    fillColor = "#a9cd07", color ='#014700') 
