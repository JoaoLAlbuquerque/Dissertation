rm(list = ls());gc()

 
# 24/08/21
# Joao Lucas 

# preciso fazer com que todos os pacotes sejam carregados automaticamente 

my.packages <- c("readr","data.table","dplyr","sf","mapview","purrr","ggplot2","tidytransit")
lapply(my.packages, library, character.only = TRUE)

list.dir.sc <- list.dirs(path = '../data-raw/Bilhetagem/Bilhetagem 2016/',full.names = T)[2:9]
list.files.sc <- list.files(path = list.dir.sc,pattern = '.csv',full.names = T)

test <- list.files.sc[1]

# x <- fread(test) 

grouped.line.day <- map_df(.x = list.files.sc, function(y){
  
  y <- fread(y)
  grouped.line.day <- y %>% group_by(linha,nome_linha,sentido_viagem,dia) %>% 
    summarise(Valids = n())
  
  grouped.line.day
  
})

write_rds(x = grouped.line.day,file = '../processed-data/grouped.line.day.rds')

grouped.line.day <- read_rds('../processed-data/grouped.line.day.rds')

# plot preliminar 

# ggplot(grouped.line.day) + geom_point(aes(y = Valids, x = dia, color = linha))


# GTFS --------------------------------------------------------------------

# before shopping 

gtfs_before <- read_gtfs(path = '../data-raw/GTFS/2016/exportacao11082016.zip')

#shopping rio mar Kennedy 
shopp <- st_read('C:\\Users\\Planejamento\\Downloads\\RIOMAR.gpkg') %>% 
  st_transform(31984) %>% st_buffer(200) %>% st_transform(4326) %>% 
  mutate(VAMO = 'OK')

mapview(shopp)

# stations before 
stations.before  <- gtfs_before$stops %>% st_as_sf(coords = c('stop_lon','stop_lat'), crs = 4326)
mapview(stations.before)# 

stations.prox.shopp <- st_join(stations.before,shopp) %>% 
  filter(is.na(VAMO) == FALSE)
mapview(stations.prox.shopp)  + mapview(shopp)

# route level 1 

lines.level.1 <- gtfs_before$stop_times %>% 
  filter(stop_id %in% stations.prox.shopp$stop_id)

cod.trip_before <- unique(lines.level.1$trip_id)

trips_before <- gtfs_before$trips %>% filter(trip_id %in% cod.trip_before)

# table with informations of all routes 
routes <- gtfs_before$routes %>% select(route_id,route_short_name,route_long_name)

cod_lines_lv1_before <- trips_before %>% distinct(route_id,shape_id)  %>% 
  left_join(routes, by = c('route_id' = 'route_id')) %>% 
  mutate(LEVEL = 1)

# route level 2

stops_level_2 <- gtfs_before$stop_times %>% filter(trip_id %in% cod.trip_before) %>% 
  pull(stop_id)

cod.trip_before.lv2 <- gtfs_before$stop_times %>% filter(stop_id %in% stops_level_2) %>% 
  pull(trip_id)

trips_before.lv2 <- gtfs_before$trips %>% filter(trip_id %in% cod.trip_before.lv2)

cod_lines_lv2_before <- trips_before.lv2 %>% distinct(route_id,shape_id)  %>% 
  left_join(routes, by = c('route_id' = 'route_id')) %>% 
  mutate(LEVEL = 2)

# route level 3 

`%nin%` = Negate(`%in%`)

codes.lines.lv1.lv2_before <- rbind(cod_lines_lv1_before,cod_lines_lv2_before) %>% pull(route_id)

codes.lines_lv3 <- routes %>% filter(route_id %nin% codes.lines.lv1.lv2_before) %>% 
  mutate(LEVEL = 0)

table_route_shapeid <- gtfs_before$trips %>% select(route_id,shape_id)

codes.lines_lv3 <- codes.lines_lv3 %>% left_join(table_route_shapeid, by = c("route_id"="route_id")) %>% 
  distinct_all(.keep_all = T) %>% 
  select(route_id,shape_id,route_short_name,route_long_name,LEVEL)

infos.linhas.before <- rbind(cod_lines_lv1_before,cod_lines_lv2_before,
                             codes.lines_lv3) 

write_rds(infos.linhas.before,file = '../processed-data/info.lines.level.before.rds')


# GTFS After  -------------------------------------------------------------

gtfs_after <- read_gtfs(path = '../data-raw/GTFS/2016/exportacao24112016.zip')

#shopping rio mar Kennedy 
shopp <- st_read('C:\\Users\\Planejamento\\Downloads\\RIOMAR.gpkg') %>% 
  st_transform(31984) %>% st_buffer(200) %>% st_transform(4326) %>% 
  mutate(VAMO = 'OK')

mapview(shopp)

# stations after 
stations.after  <- gtfs_after$stops %>% st_as_sf(coords = c('stop_lon','stop_lat'), crs = 4326)
mapview(stations.after)# 

stations.prox.shopp <- st_join(stations.after,shopp) %>% 
  filter(is.na(VAMO) == FALSE)
mapview(stations.prox.shopp)  + mapview(shopp)

# route level 1 

lines.level.1 <- gtfs_after$stop_times %>% 
  filter(stop_id %in% stations.prox.shopp$stop_id)

cod.trip_after <- unique(lines.level.1$trip_id)

trips_after <- gtfs_after$trips %>% filter(trip_id %in% cod.trip_after)

# table with informations of all routes 
routes <- gtfs_after$routes %>% select(route_id,route_short_name,route_long_name)

cod_lines_lv1_after <- trips_after %>% distinct(route_id,shape_id)  %>% 
  left_join(routes, by = c('route_id' = 'route_id')) %>% 
  mutate(LEVEL = 1)

# route level 2

stops_level_2 <- gtfs_after$stop_times %>% filter(trip_id %in% cod.trip_after) %>% 
  pull(stop_id)

cod.trip_after.lv2 <- gtfs_after$stop_times %>% filter(stop_id %in% stops_level_2) %>% 
  pull(trip_id)

trips_after.lv2 <- gtfs_after$trips %>% filter(trip_id %in% cod.trip_after.lv2)

cod_lines_lv2_after <- trips_after.lv2 %>% distinct(route_id,shape_id)  %>% 
  left_join(routes, by = c('route_id' = 'route_id')) %>% 
  mutate(LEVEL = 2)

# route level 3 

`%nin%` = Negate(`%in%`)

codes.lines.lv1.lv2_after <- rbind(cod_lines_lv1_after,cod_lines_lv2_after) %>% pull(route_id)

codes.lines_lv3 <- routes %>% filter(route_id %nin% codes.lines.lv1.lv2_after) %>% 
  mutate(LEVEL = 0)

table_route_shapeid <- gtfs_after$trips %>% select(route_id,shape_id)

codes.lines_lv3 <- codes.lines_lv3 %>% left_join(table_route_shapeid, by = c("route_id"="route_id")) %>% 
  distinct_all(.keep_all = T) %>% 
  select(route_id,shape_id,route_short_name,route_long_name,LEVEL)

infos.linhas.after <- rbind(cod_lines_lv1_after,cod_lines_lv2_after,
                             codes.lines_lv3) 

write_rds(infos.linhas.after,file = '../processed-data/info.lines.level.after.rds')

