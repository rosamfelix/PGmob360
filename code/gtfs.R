# objetivo: ler dados de um arquivo GTFS e visualizar as paragens e rotas

# Pacotes
# install.packages("tidytransit")
library(tidyverse)
library(tidytransit)
library(mapview)

# Ler dados
METRO = read_gtfs("original/metro_gtfs.zip")
validate_gtfs(METRO) # validar

# Ver os dados

# Paragens
View(METRO$stops)

ESTACOES_metro = stops_as_sf(METRO$stops) # converter texto para geometria
# ESTACOES_metro = ESTACOES_metro |> filter(parent_station == "") # remover aquelas subsidiarias
mapview(ESTACOES_metro) # visualizar

## Desafio ##
# Ver que estações de metro se consegue alcançar em 15, 30 e 45 minutos a partir da Baixa
# 1. num dia útil em hora de ponta
# 2. num domingo de tarde


# 1. num dia útil em hora de ponta

# filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
stop_times_metro = filter_stop_times(METRO, "2024-11-20", "07:30:00", "09:00:00")
duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa

summary(duracao_viagem$travel_time/60) # resumo em minutos

duracao_viagem_BC_HP = ESTACOES_metro |> 
  left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(!is.na(travel_time)) # remover as plataformas não usadas

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_BC_HP) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde a estação Baixa-Chiado",
          subtitle = "às 7h30 de Quarta, 20 Nov 2024") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()

# 2. num domingo de tarde
# filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
stop_times_metro = filter_stop_times(METRO, "2024-11-24", "21:30:00", "22:30:00")
duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa

summary(duracao_viagem$travel_time/60) # resumo em minutos

duracao_viagem_BC_Dom = ESTACOES_metro |> 
  left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(!is.na(travel_time)) # remover as plataformas não usadas

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_BC_Dom) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde a estação Baixa-Chiado",
          subtitle = "às 21h30 de Domingo, 24 Nov 2024") +
  labs(color = "Tempo de viagem [min]") +
  theme_bw()



# Carris ------------------------------------------------------------------

# Ler dados
CARRIS = read_gtfs("original/carris_metropolitana.zip")
validate_gtfs(CARRIS) # validar

View(CARRIS$stops)

# Paragens
ESTACOES_carris = stops_as_sf(CARRIS$stops) # converter texto para geometria
mapview(ESTACOES_carris) # visualizar
mapview(ESTACOES_carris, zcol = "has_shelter") # por tipo de abrigo
mapview(ESTACOES_carris, zcol = "municipality_name") # por concelho

# Percursos
ROTAS_carris = tidytransit::shapes_as_sf(CARRIS$shapes) # converter texto para geometria
plot(ROTAS_carris) # ver as 1739 rotas
mapview(ROTAS_carris, zcol = "shape_id") # visualizar

# 1. num dia útil em hora de ponta

# # filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
# stop_times_metro = filter_stop_times(METRO, "2024-11-20", "07:30:00", "09:00:00")
# duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa
# 
# summary(duracao_viagem$travel_time/60) # resumo em minutos
# 
# duracao_viagem_BC_HP = ESTACOES_metro |> 
#   left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
#   filter(!is.na(travel_time)) # remover as plataformas não usadas
# 
# # mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
# ggplot(duracao_viagem_BC_HP) +
#   geom_sf(aes(color = travel_time/60))+
#   ggtitle("Alcance desde a estação Baixa-Chiado",
#           subtitle = "às 7h30 de Quarta, 20 Nov 2024") +
#   labs(color = "Tempo de viagem [min]") +
#   theme_bw()
# 
# # 2. num domingo de tarde
# # filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
# stop_times_metro = filter_stop_times(METRO, "2024-11-24", "21:30:00", "22:30:00")
# duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa
# 
# summary(duracao_viagem$travel_time/60) # resumo em minutos
# 
# duracao_viagem_BC_Dom = ESTACOES_metro |> 
#   left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
#   filter(!is.na(travel_time)) # remover as plataformas não usadas
# 
# # mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
# ggplot(duracao_viagem_BC_Dom) +
#   geom_sf(aes(color = travel_time/60))+
#   ggtitle("Alcance desde a estação Baixa-Chiado",
#           subtitle = "às 21h30 de Domingo, 24 Nov 2024") +
#   labs(color = "Tempo de viagem [min]") +
#   theme_bw()
