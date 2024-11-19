# data preparation from Metro

# load libraries
library(dplyr)
library(readr)


# Estação X ---------------------------------------------------------------

# load original data
DADOSabrilX = read_delim("original/Estacao X - Dados de Abril 2024 (1 079 268 registos).csv", 
                                                      delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                                      trim_ws = TRUE)
DADOSabrilX = DADOSabrilX |>
  select(Dia, Hora, Título, `N. de série`, Entrada, Saída) |> 
  rename(Titulo = Título,
         ID = `N. de série`,
         Saida = Saída) |> 
  na.omit() |> 
  mutate(
    Dia = ymd(Dia),
    hour = hour(Hora),
    Tipo_dia = if_else(wday(Dia) %in% c(1, 7), "Fim-de-Semana", "Dia-util") # 1 = Sunday, 7 = Saturday
  ) |> 
  select(Dia, Hora, hour, Tipo_dia, Titulo, ID, Entrada, Saida)


DADOSmaioX = read_delim("original/Estacao X - Dados de Maio 2024 (1 129 439 registos).csv", 
                        delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                        trim_ws = TRUE)
DADOSmaioX = DADOSmaioX |>
  select(Dia, Hora, Título, `N.º de Série`, Entrada, Saída) |> 
  rename(Titulo = Título,
         ID = `N.º de Série`,
         Saida = Saída) |> 
  na.omit() |> 
  mutate(
    Dia = ymd(Dia),
    hour = hour(Hora),
    Tipo_dia = if_else(wday(Dia) %in% c(1, 7), "Fim-de-Semana", "Dia-util") # 1 = Sunday, 7 = Saturday
  ) |> 
  select(Dia, Hora, hour, Tipo_dia, Titulo, ID, Entrada, Saida)

# DADOSmetroX = rbind(DADOSabrilX, DADOSmaioX)

# save data
# write.csv(DADOSmetroX, "original/DADOSmetroX.csv", row.names = FALSE)
write.csv(DADOSabrilX, "original/DADOSabrilX.csv", row.names = FALSE)
write.csv(DADOSmaioX, "original/DADOSmaioX.csv", row.names = FALSE)
piggyback::pb_upload("original/DADOSabrilX.csv")




# Estação Y ---------------------------------------------------------------

# load original data
DADOSabrilY = read_delim("original/Estacao Y - Dados de Abril 2024 (857 979 registos).csv", 
                         delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                         trim_ws = TRUE)
DADOSabrilY = DADOSabrilY |>
  select(Dia, Hora, Título, `n. Série`, Entrada, Saída) |> 
  rename(Titulo = Título,
         ID = `n. Série`,
         Saida = Saída) |> 
  na.omit() |> 
  mutate(
    Dia = ymd(Dia),
    hour = hour(Hora),
    Tipo_dia = if_else(wday(Dia) %in% c(1, 7), "Fim-de-Semana", "Dia-util") # 1 = Sunday, 7 = Saturday
  ) |> 
  select(Dia, Hora, hour, Tipo_dia, Titulo, ID, Entrada, Saida)


DADOSmaioY = read_delim("original/Estacao Y - Dados de Maio 2024 (875 843 registos).csv", 
                        delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                        trim_ws = TRUE)
DADOSmaioY = DADOSmaioY |>
  select(Dia, Hora, Título, `n. Série`, Entrada, Saída) |> 
  rename(Titulo = Título,
         ID = `n. Série`,
         Saida = Saída) |> 
  na.omit() |> 
  mutate(
    Dia = ymd(Dia),
    hour = hour(Hora),
    Tipo_dia = if_else(wday(Dia) %in% c(1, 7), "Fim-de-Semana", "Dia-util") # 1 = Sunday, 7 = Saturday
  ) |> 
  select(Dia, Hora, hour, Tipo_dia, Titulo, ID, Entrada, Saida)

# DADOSmetroY = rbind(DADOSabrilY, DADOSmaioY)

# save data
# write.csv(DADOSmetroY, "original/DADOSmetroY.csv", row.names = FALSE)
write.csv(DADOSabrilY, "original/DADOSabrilY.csv", row.names = FALSE)
write.csv(DADOSmaioY, "original/DADOSmaioY.csv", row.names = FALSE)
piggyback::pb_upload("original/DADOSabrilY.csv")





# titulos transporte --------------------------------------------------------------------------

DADOSmetro = rbind(DADOSabrilX, DADOSmaioX, DADOSabrilY, DADOSmaioY)

Titulos = read_delim("original/Títulos (2 654 registos).csv", 
                     delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                     trim_ws = TRUE)

# ver apenas os que são utilizados nestes dados
Titulos = DADOSmetro |> select(Titulo) |> distinct() |>
  left_join(Titulos |> select(Título, Texto), by = c("Titulo" = "Título")) |>
  distinct()

# reclassificar
Titulos = Titulos |>
  mutate(
    Tipo =
      case_when(
        grepl("S23", Texto, ignore.case = TRUE) ~ "Sub23",
        grepl("Navegante Lisboa", Texto, ignore.case = TRUE) ~ "Navegante Lisboa",
        grepl("Navegante Odivelas", Texto, ignore.case = TRUE) ~ "Navegante Odivelas",
        grepl("Navegante", Texto, ignore.case = TRUE) ~ "Navegante Metropolitano", # inclui Urbano
        grepl("sub23", Texto, ignore.case = TRUE) ~ "Sub23",
        grepl("BT", Texto, ignore.case = TRUE) ~ "Carris Metropolitana",
        grepl("30", Texto, ignore.case = TRUE) ~ "Combinado Mensal",
        grepl("Trab", Texto, ignore.case = TRUE) ~ "Trabalhador",
        grepl("24", Texto, ignore.case = TRUE) ~ "Diario",
        grepl("48H", Texto, ignore.case = TRUE) ~ "Diario",
        grepl("72", Texto, ignore.case = TRUE) ~ "Diario",
        grepl("Zapping", Texto, ignore.case = TRUE) ~ "Zapping",
        grepl("Bilhete", Texto, ignore.case = TRUE) ~ "Viagem",
        grepl("Viagem", Texto, ignore.case = TRUE) ~ "Viagem",
        grepl("Comb", Texto, ignore.case = TRUE) ~ "Combinado Mensal",
        TRUE ~ "Outro"
      )
  ) |> 
  mutate(Tipo_redux =
           case_when(
             grepl("Sub23", Tipo, ignore.case = TRUE) ~ "Mensal",
             grepl("Navegante", Tipo, ignore.case = TRUE) ~ "Mensal",
             grepl("Carris", Tipo, ignore.case = TRUE) ~ "Mensal",
             grepl("Combinado", Tipo, ignore.case = TRUE) ~ "Mensal",
             grepl("Diario", Tipo, ignore.case = TRUE) ~ "Vulso",
             grepl("Zapping", Tipo, ignore.case = TRUE) ~ "Vulso",
             grepl("Viagem", Tipo, ignore.case = TRUE) ~ "Vulso",
             grepl("Trabalhador", Tipo, ignore.case = TRUE) ~ "Mensal",
             TRUE ~ "Outro"
           )
  )

  

Titulos_simplex = Titulos |>
  select(Titulo, Tipo, Tipo_redux) |>
  distinct(Titulo, .keep_all = TRUE) |> 
  arrange(Tipo)

table(Titulos_simplex$Tipo)
table(Titulos_simplex$Tipo_redux)

# save data
write.csv(Titulos_simplex, "data/Titulos.csv", row.names = FALSE)

rm(DADOSmetro, DADOSmetroX, DADOSmetroY)
