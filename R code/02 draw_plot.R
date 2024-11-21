library(tmap)
library(colorspace)

# Origin and destination volumes
point_org <- data_grain %>%
  group_by(origin, org) %>% 
  summarise(vahoniv = sum(vahoniv)) %>% 
  arrange(desc(vahoniv)) %>% 
  left_join(stations_grain, by=c('org'='id')) %>% 
  st_as_sf()

point_dst <- data_grain %>%
  group_by(destination, dst) %>% 
  summarise(vahoniv = sum(vahoniv)) %>% 
  arrange(desc(vahoniv)) %>% 
  left_join(stations_grain, by=c('dst'='id')) %>% 
  st_as_sf()

# Plots
water <- ne_download(scale='large', type='lakes', 
                     category='physical', returnclass = 'sf') %>% 
  st_make_valid() %>% 
  st_intersection(ukraine)

dnipro <- ne_download(scale='large', type='rivers_lake_centerlines', 
                      category='physical', returnclass = 'sf') %>% 
  filter(name_en == "Dnieper") %>% 
  st_intersection(ukraine)

tmap_mode("plot")
(g2 <- tm_shape(ukraine) + tm_polygons(border.alpha = 0.3) +
    tm_shape(dnipro) + tm_lines(col='white', lwd=1.5) +
    tm_shape(water) + tm_polygons(col='white', border.alpha = 0.3) +
    tm_shape(flow_agg_sf %>% st_intersection(ukraine)) + 
    tm_lines(col='#fecc5c', lwd='vahoniv', scale=10,
             title.lwd= "flow, '000 railcars",
             lwd.legend = c(50000, 100000, 200000, 300000), 
             lwd.legend.labels = c("50","100","200","300")) +
    tm_shape(point_org) + 
    tm_bubbles(size='vahoniv', size.max=350000, scale=4, alpha=0.4, 
               col=lighten('#feeb75',0.5),
               border.col='#feeb75', border.lwd=1,
               title.size = "origin, '000 railcars",
               sizes.legend = c(1000, 10000, 25000, 100000), 
               sizes.legend.labels = c("1","10","25","100")) +
    tm_shape(point_dst) + 
    tm_bubbles(size='vahoniv', size.max= 350000, scale=4, alpha=0.4, 
               col=lighten('#fd8d3c',0.5),
               border.col='#fd8d3c', border.lwd=1,
               title.size = "destination, '000 railcars",
               sizes.legend = c(1000, 10000, 25000, 100000), 
               sizes.legend.labels = c("1","10","25","100")) +
    tm_layout(fontfamily = "Roboto Condensed", 
              main.title = "Grain flows by rail in Ukraine, 2020",
              legend.outside = TRUE,
              frame = FALSE) +
    tm_credits("Data source: Opendata.gov.ua",
               position = c("left", "bottom"), just = "left")
)

tmap_save(g2, './plots/grain_routes v2.png')
