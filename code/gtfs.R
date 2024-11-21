# objetivo: ler dados de um arquivo GTFS e visualizar as paragens e rotas

## Pacotes
# install.packages("tidytransit")
library(tidyverse)
library(tidytransit)
library(mapview)

# Limite município Lisboa
MunicipiosGEO = sf::st_read("data/Municipalities_geo.gpkg")
LisboaGEO = MunicipiosGEO |> filter(Municipality == "Lisboa")

# Metro Lisboa ------------------------------------------------------------

# Ler dados GTFS
METRO = read_gtfs("https://www.metrolisboa.pt/google_transit/googleTransit.zip") # diretamente do site Metro
validate_gtfs(METRO) # validar

# Ver os dados
View(METRO)
View(METRO$stops)

ESTACOES_metro = stops_as_sf(METRO$stops) # converter texto para geometria
# ESTACOES_metro = ESTACOES_metro |> filter(parent_station == "") # remover aquelas subsidiarias
mapview(ESTACOES_metro) # visualizar

## Desafio ##
# Ver quanto tempo demora a alcançar cada estação a partir da Baixa
# 1. num dia útil em hora de ponta
# 2. num domingo de tarde

# 1. num dia útil em hora de ponta
# filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
stop_times_metro = filter_stop_times(METRO, "2024-11-20", "07:30:00", "09:00:00")
duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa

summary(duracao_viagem$travel_time/60) # resumo em minutos



# 2. num domingo de tarde
# filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
stop_times_metro = filter_stop_times(METRO, "2024-11-24", "21:30:00", "22:30:00")
duracao_viagem = travel_times(stop_times_metro, "Baixa / Chiado") # Baixa / Chiado é a estação de metro da Baixa

summary(duracao_viagem$travel_time/60) # resumo em minutos


# mapa com o tempo que demora a chegar a cada estação em hora de ponta
duracao_viagem_BC_HP = ESTACOES_metro |> 
  left_join(duracao_viagem, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(!is.na(travel_time)) # remover as plataformas não usadas

ggplot(duracao_viagem_BC_HP) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde a estação Baixa-Chiado (Metro)",
          subtitle = "às 7h30 de Quarta, 20 Nov 2024") +
  labs(color = "Tempo de viagem [min]") +
  geom_sf(data = LisboaGEO,
          fill = "transparent",
          color = "grey30") +
  theme_bw()



# Carris Lisboa -----------------------------------------------------------

CARRIS = read_gtfs("https://github.com/rosamfelix/PGmob360/releases/download/2024.11/carris_gtfs.zip")
validate_gtfs(CARRIS) # validar
# foi criado um ficheiro transfers.txt que não existia no original, com o gtfsrouter

# Paragens
View(CARRIS$stops)
ESTACOES_carris = stops_as_sf(CARRIS$stops) # converter texto para geometria
mapview(ESTACOES_carris) # visualizar

# Percursos
ROTAS_carris = tidytransit::shapes_as_sf(CARRIS$shapes) # converter texto para geometria
plot(ROTAS_carris) # ver as 1739 rotas
mapview(ROTAS_carris, zcol = "shape_id") # visualizar


# 1. num dia útil em hora de ponta, a partir do Cais do Sodré

# filtramos os dados para o dia de hoje entre as 7h30 e as 9h00
stop_times_carris1 = filter_stop_times(CARRIS, "2024-11-20", "07:30:00", "09:00:00")
duracao_viagem1 = travel_times(stop_times_carris1, "Cais Sodré", stop_dist_check = FALSE)

# estatísticas
summary(duracao_viagem1$travel_time/60) # resumo em minutos
hist(duracao_viagem1$travel_time/60, breaks = 20) # histograma
abline(v = mean(duracao_viagem1$travel_time/60), col = "red") # linha de média
abline(v = median(duracao_viagem1$travel_time/60), col = "blue") # linha de mediana
round(prop.table(table(duracao_viagem1$transfers))*100, 1) # percentagem de transferências

duracao_viagem_CS_HP = ESTACOES_carris |> 
  left_join(duracao_viagem1, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(transfers >= 1) |> # remover viagens com mais de 1 transferência
  filter(!is.na(travel_time)) |>  # remover as plataformas não usadas
  filter(travel_time < 60*60) |> # remover viagens com mais de 60 minutos
  filter(!stop_id %in% c(19001,14601,14602)) # remover as pagarens em Almada
  
# mapa com as estações que se conseguem alcançar em contínuo
ggplot(duracao_viagem_CS_HP) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde o Cais do Sodré (Carris)",
          subtitle = "às 7h30 de Quarta, 20 Nov 2024 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  geom_sf(data = LisboaGEO,
          fill = "transparent",
          color = "grey30") +
  theme_bw()

# 2. num domingo de tarde
# filtramos os dados para o próximo Domingo entre as 21h30 e as 22h30
stop_times_carris2 = filter_stop_times(CARRIS, "2024-11-24", "22:00:00", "23:30:00")
duracao_viagem2 = travel_times(stop_times_carris2, "Cais Sodré", stop_dist_check = FALSE)

summary(duracao_viagem2$travel_time/60) # resumo em minutos

duracao_viagem_CS_Dom = ESTACOES_carris |> 
  left_join(duracao_viagem2, by = c("stop_id" = "to_stop_id")) |>  # juntar os dados 
  filter(transfers >= 1) |> # remover viagens com mais de 1 transferência
  filter(!is.na(travel_time)) |>  # remover as plataformas não usadas
  filter(travel_time < 60*60) |>  # remover viagens com mais de 60 minutos
  mutate(intervalo = case_when(
    travel_time < 15*60 ~ "até 15 min",
    travel_time < 30*60 ~ "até 30 min",
    travel_time < 45*60 ~ "até 45 min",
    TRUE ~ "até 60 min"
  )) # criar uma variável com o intervalo de tempo

# mapa com escala contínua de tempo
ggplot(duracao_viagem_CS_Dom) +
  geom_sf(aes(color = travel_time/60))+
  ggtitle("Alcance desde o Cais do Sodré (Carris)",
          subtitle = "às 22h00 de Domingo, 24 Nov 2024 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  geom_sf(data = LisboaGEO,
          fill = "transparent",
          color = "grey30") +
  theme_bw()

# mapa com as estações que se conseguem alcançar em 15, 30 e 45 minutos
ggplot(duracao_viagem_CS_Dom) +
  geom_sf(aes(color = intervalo))+ # mudar para escala discreta
  scale_color_manual(values = c("até 15 min" = "#119da4",
                                "até 30 min" = "#0c7489", 
                                "até 45 min" = "#13505B",
                                "até 60 min" = "#040404")) +
  ggtitle("Alcance desde o Cais do Sodré (Carris)",
          subtitle = "às 22h00 de Domingo, 24 Nov 2024 - máx 1 transf") +
  labs(color = "Tempo de viagem [min]") +
  geom_sf(data = LisboaGEO,
          fill = "transparent",
          color = "grey30") +
  theme_bw()




# Carris Metropolitana ------------------------------------------------------------------

# Ler dados
CARRISm = read_gtfs("https://github.com/rosamfelix/PGmob360/releases/download/2024.11/carris_metropolitana.zip")
validate_gtfs(CARRISm) # validar
# foi criado um ficheiro transfers.txt que não existia no original


# Paragens
View(CARRISm$stops)
ESTACOES_carrism = stops_as_sf(CARRISm$stops) # converter texto para geometria
mapview(ESTACOES_carrism) # visualizar
mapview(ESTACOES_carrism, zcol = "has_shelter") # por tipo de abrigo
mapview(ESTACOES_carrism, zcol = "municipality_name") # por concelho

# Percursos
ROTAS_carrism = tidytransit::shapes_as_sf(CARRISm$shapes) # converter texto para geometria
mapview(ROTAS_carrism, zcol = "shape_id") # visualizar

