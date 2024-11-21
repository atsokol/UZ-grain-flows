library(tidyverse)
library(sf)
library(rnaturalearth)
library(sfnetworks)
library(purrr)
library(lubridate)
library(lwgeom)
library(geojsonio)

# Ukraine map with Crimea (Natural Earth must correct its maps!)
ukraine <- ne_states(country="Ukraine", returnclass="sf")
crimea <- ne_states(country='russia', returnclass = 'sf') %>% filter(name == 'Crimea')
ukraine <- rbind(ukraine, crimea) %>% st_union(by_feature=FALSE)
rm(crimea)

# Load flow and station data
data_clean <- read_csv("data/input data/flows.csv", col_types = "cccddddDii")
stations_sf <- read_csv("data/input data/stations.csv", col_types = "cidd") |> 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))

# Load rail network data from Natural Earth
rail_network <- st_read("./data/Shapefiles/ne_10m_railroads/ne_10m_railroads.shp") %>% 
  filter(continent %in% c('Europe', 'Asia')) %>% 
  st_crop(ukraine) %>% 
  filter(st_geometry_type(geometry) == 'LINESTRING')

# =====================
# Grain data 2020

# Extract data for grain
data_grain <- data_clean %>% 
  mutate(year = year(date)) %>% 
  filter(cargo == "зерно,прод.перем.",
         year == 2020) %>% 
  group_by(origin, destination, org, dst) %>% 
  summarise(vahoniv = sum(vahoniv),
            ton = sum(ton)) %>% 
  ungroup()

# Calculate unique routes for grain
stations_grain <- stations_sf[c(data_grain$org, data_grain$dst) %>% unique() %>% sort(), ] %>% 
  st_crop(ukraine)

rts <- data_grain %>% 
  select(org, dst) %>% 
  unique() %>% 
  mutate(id = row_number()) %>% 
  filter(org %in% stations_grain$id,
         dst %in% stations_grain$id)

# Blend stations into rail network
net <- as_sfnetwork(rail_network, directed = FALSE, ) %>% 
  st_network_blend(stations_grain)
  
# Define function to calculate shortest paths on a graph
find_path <- function(from, to){
  path = st_network_paths(net, stations_sf[from, ], stations_sf[to, ], use_names = FALSE)
}

# Calculate shortest paths and append to data frame
paths <- map2_dfr(rts$org, rts$dst, find_path) 

rts <- rts |> 
  mutate(path = paths$edge_paths,
         from = paths$node_paths,
         to = paths$node_paths)
  
rts$from <- lapply(rts$from, function(x) x[-length(x)])
rts$to <- lapply(rts$to, function(x) x[-1])
  
# Aggregate flows to an undirected graph
flow_agg <- data_clean %>% 
  filter(cargo == "зерно,прод.перем.") %>%
  inner_join(rts, by=c("org"="org", "dst"="dst")) %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2020) %>% 
  unnest() %>% 
  group_by(year, path) %>% 
  summarise(vahoniv = sum(vahoniv),
            ton = sum(ton)) %>% 
  arrange(path)

flow_agg_sf <- net %>% 
  activate('edges') %>% 
  mutate(id = row_number()) %>% 
  slice(flow_agg$path) %>% 
  st_as_sf() %>% 
  left_join(flow_agg, by=c("id" = "path")) %>% 
  select(id, year, vahoniv, ton)

st_write(flow_agg_sf, "./data/_export data/grain flows agg.geojson")

# Aggregate flows to a directed graph
flow_agg_dir <- data_clean %>% 
  filter(cargo == "зерно,прод.перем.") %>%
  inner_join(rts, by=c("org"="org", "dst"="dst")) %>% 
  mutate(year = year(date)) %>% 
  filter(year == 2020) %>% 
  unnest() %>% 
  group_by(year, path, from, to) %>% 
  summarise(vahoniv = sum(vahoniv),
            ton = sum(ton)) %>% 
  arrange(path)

flow_agg_dir_sf <- net |> 
  activate('edges') |> 
  mutate(id = row_number()) |> 
  slice(flow_agg_dir$path) |> 
  st_as_sf() |> 
  right_join(flow_agg_dir, by=c("id" = "path")) |> 
  mutate(geometry = case_when(
    from.x == from.y ~ geometry,
    from.x == to.y ~ st_reverse(geometry)
  )) |> 
  select(from = from.y, to = to.y, year, vahoniv, ton)

st_write(flow_agg_dir_sf, "./data/_export data/grain flows agg dir.geojson")
topojson_write(flow_agg_dir_sf |> mutate(idf = row_number()), file = "./data/_export data/grain flows agg dir.topojson")

# File with nodes
nodes_dir_sf <- net |> 
  activate('nodes') |> 
  mutate(id = row_number()) |> 
  slice(c(flow_agg_dir$from, flow_agg_dir$to)) |> 
  st_as_sf() |> 
  select(id, name)
  
st_write(nodes_dir_sf, "./data/_export data/stations dir.geojson")
topojson_write(nodes_dir_sf, file = "./data/_export data/stations dir.topojson")

# Create csv files
nodes_dir <- nodes_dir_sf |> 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2]) |> 
  st_drop_geometry()

st_write(nodes_dir, "./data/_export data/stations dir.csv")

st_write(st_drop_geometry(flow_agg_dir_sf), "./data/_export data/grain flows agg dir.csv")


