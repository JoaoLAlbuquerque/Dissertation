rm(list = ls());gc()

# 24/08/21
# Joao Lucas 

# preciso fazer com que todos os pacotes sejam carregados automaticamente 

my.packages <- c("readr","data.table","dplyr","sf","mapview","purrr","ggplot2","tidytransit")
lapply(my.packages, library, character.only = TRUE)

grouped.line.day <- read_rds('../processed-data/grouped.line.day.rds') %>% 
  mutate(linha = as.character(linha))

info.lines.before <- read_rds('../processed-data/info.lines.level.before.rds') %>% 
  mutate(LEVEL.str = case_when(LEVEL == 0 ~ '3 - no acess',
                               LEVEL == 1 ~ '1 - direct acess',
                               LEVEL == 2 ~ '2 - intermediary acess'))

qtd.before <-info.lines.before %>% group_by(LEVEL) %>% summarise(N = n())
  
info.lines.after <- read_rds('../processed-data/info.lines.level.after.rds') %>% 
  mutate(LEVEL.str = case_when(LEVEL == 0 ~ '3 - no acess',
                               LEVEL == 1 ~ '1 - direct acess',
                               LEVEL == 2 ~ '2 - intermediary acess'))

qtd.after <-info.lines.after%>% group_by(LEVEL) %>% summarise(N = n())


# assumption - gtfs before works until intervention 

opening_date <- "2016-10-26"

data.before <- grouped.line.day %>% filter(dia < opening_date)
data.after <- grouped.line.day %>% filter(dia > opening_date)

data.before.levels <- data.before %>% left_join(info.lines.before, by = c("linha"="route_short_name")) %>% 
  distinct(nome_linha,sentido_viagem,dia,.keep_all = T)
# several lines has NA values 
# this is because data differences between GTFS and smart card 

teste.count <- data.before.levels %>% group_by(linha,LEVEL.str) %>% summarise(N = n())

resume.before <- data.before.levels %>% group_by(LEVEL.str,dia) %>% summarise(Ridership = sum(Valids)) %>% 
  na.omit() %>% filter(dia > "2016-02-01") %>% 
  mutate(date = lubridate::ymd(dia))

data.after.levels <- data.after %>% left_join(info.lines.after, by = c("linha"="route_short_name")) %>% 
  distinct(nome_linha,sentido_viagem,dia,.keep_all = T)

resume.after <- data.after.levels %>% group_by(LEVEL.str,dia) %>% summarise(Ridership = sum(Valids)) %>% 
  na.omit() %>% filter(dia > "2016-02-01") %>% 
  filter(dia < "2017-02-01") %>% 
  mutate(date = lubridate::ymd(dia))

resumo.tot <- rbind(resume.before,resume.after) %>% filter(dia < "2016-12-1")

resumo.linha <- rbind(data.before.levels,data.after.levels)  %>% 
  na.omit() %>% group_by(linha,LEVEL.str,dia) %>% summarise(Ridership = sum(Valids)) %>% 
  filter(dia > "2016-02-01") %>% 
  filter(dia < "2017-02-01")

teste.no.acess <- resumo.linha %>% filter(LEVEL.str == '1 - direct acess')

ggplot(resumo.linha %>% filter(LEVEL.str == '1 - direct acess')) + 
  geom_point(aes(y = Ridership, x = dia, color = as.factor(linha)))  +
  geom_smooth(aes(y = Ridership, x = dia, color = as.factor(linha)))
  geom_line(aes(y = Ridership, x = dia, group = linha))

ggplot(resume.before) + geom_point(aes(y = Ridership, x = date, color = as.factor(LEVEL.str)), size = 1) + 
  geom_smooth(aes(y = Ridership, x = date, color = as.factor(LEVEL.str))) + theme_bw() +
  geom_point(data = resume.after,aes(y = Ridership, x = date, color = as.factor(LEVEL.str)), size = 1) + 
  geom_smooth(data = resume.after,aes(y = Ridership, x = date, color = as.factor(LEVEL.str)))+
  labs(color = 'Shopping acess') + 
  geom_vline(xintercept = lubridate::ymd(opening_date),linetype = 2, size = 1, color = 'red') + 
  geom_text(label = 'Opening date - shopping',x = lubridate::ymd("2016-11-29"), y = 1700000, color = 'red') + 
scale_color_manual(values = c('#2B32AA','grey60','#07D203'))


ggplot(resumo.tot) + geom_point(aes(y = Ridership, x = date, color = as.factor(LEVEL.str)), size = 1) + 
  geom_smooth(aes(y = Ridership, x = date, color = as.factor(LEVEL.str))) + theme_bw() +
  labs(color = 'Shopping acess') + 
  geom_vline(xintercept = lubridate::ymd(opening_date),linetype = 2, size = 1, color = 'red') + 
  geom_text(label = 'Opening date - shopping',x = lubridate::ymd("2016-10-20"), y = 1600000, color = 'red') + 
  scale_color_manual(values = c('#2B32AA','grey60','#07D203'))
