# library(tidyverse)
library(dplyr)


# ler os dados diretamente em formato R dataset
TRIPS = readRDS("data/TRIPSorigem.Rds")

glimpse(TRIPS)

TRIPS_new = select(TRIPS, Origem, Walk, Bike, Total) # o primeiro argumento é a base de dados

TRIPS_new = select(TRIPS_new, -Total) # deixar cair a variável Total

TRIPS_new = TRIPS |> select(Origem, Walk, Bike, Total)

TRIPS2 = TRIPS[TRIPS$Total > 25000,] # usando o r-base, não se pode esquecer a vírgula
TRIPS2 = TRIPS2 |> filter(Total > 25000) # usando o dplyr, é mais fácil

summary(TRIPS$Total)
TRIPS3 = TRIPS |> filter(Total > median(Total)) 

TRIPS$Car_perc = TRIPS$Car/TRIPS$Total * 100 # com o r-base

TRIPS = TRIPS |> mutate(Car_perc = Car/Total * 100) # com o dplyr

class(TRIPS$Origem)
TRIPS = TRIPS |> 
  mutate(Origem_num = as.integer(Origem)) # também podemos usar as.numeric()
class(TRIPS$Origem_num)

TRIPS = TRIPS |> 
  mutate(Lisboa_factor = factor(Lisboa, labels = c("Não", "Sim")),
         Interna_factor = factor(Interna, labels = c("Externa", "Interna")))

unique(TRIPS$Lisboa) # isto mostra todos os valores únicos (diferentes)
table(TRIPS$Lisboa) # isto mostra a frequência de cada valor
table(TRIPS$Lisboa_factor)

plot(TRIPS$Lisboa) # os valores variam entre 0 e 1
plot(TRIPS$Lisboa_factor) # os valores são categóricos e com etiquetas Sim/Não

Municipios = readRDS("data/Municipios_nomes.Rds")

head(TRIPS)
tail(Municipios)

TRIPSjoin = TRIPS |> left_join(Municipios, by = c("Origem" = "Dicofre"))

Municipios = Municipios |> rename(Origem = "Dicofre") # alterar o nome da variável
TRIPSjoin = TRIPS |> left_join(Municipios) # detecta automativamente a variável comum

TRIPSredux = TRIPSjoin |> select(Origem, Concelho, Interna, Car, Total)
head(TRIPSredux)

TRIPSsum = TRIPSredux |> 
  group_by(Concelho) |> # não irá notar nenhuma alteração na tabela
  summarize(Total = sum(Total))
head(TRIPSsum)

TRIPSsum2 = TRIPSredux |> 
  group_by(Concelho, Interna) |> 
  summarize(Total = sum(Total),
            Car = sum(Car))
head(TRIPSsum2)

TRIPS2 = TRIPSsum2 |> arrange(Total)
TRIPS2 = TRIPSsum2 |> arrange(-Total) # descendente

TRIPS2 = TRIPSsum2 |> arrange(Concelho) # alfabética

TRIPS4 = TRIPS |> arrange(Lisboa_factor, Total) # mais de uma variável

TRIPS_pipes = TRIPS |> 
  select(Origem, Interna, Car, Total) |> 
  mutate(Dicofre = Origem) |> 
  mutate(Interna_factor = factor(Interna, labels = c("Externa", "Interna"))) |> 
  filter(Interna_factor == "Interna")|>
  left_join(Municipios) |>
  group_by(Concelho) |>
  summarize(Total = sum(Total),
            Car = sum(Car),
            Car_perc = Car/Total * 100) |> 
  ungroup() |> 
  arrange(desc(Car_perc))

TRIPS_pipes

pivot = data.frame(Origens = c("A", "A", "B", "C", "C"),
                   Destinos = c("B", "C", "A", "C", "A"),
                   Viagens = c(20, 45, 10, 5, 30))
pivot


matrix = pivot |> 
  tidyr::pivot_wider(names_from = Destinos,
                                     values_from = Viagens,
                                     names_sort = TRUE) |> 
  dplyr::rename(Viagens = "Origens")
matrix

