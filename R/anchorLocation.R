#================================================================================
# Generate Anchor Locations using ggmap
# Author: Zhihang Dong
# Last Update: 10/18/2018
#================================================================================

library(devtools)
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
library(ggplot2)

register_google(key = "Your API Key")
lausanne <- get_map("lausanne", zoom = 14)

LausanneMap <- ggmap("lausanne", extent = "device", legend = "topleft")
LausanneMap +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = p.6180,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon",
  data = p.6180
)
LausanneMap + overlay + inset(
  grob = ggplotGrob(ggplot() + overlay + theme_inset()),
  xmin = 6.6323, xmax = Inf, ymin = -Inf, ymax = 46.5197
)
