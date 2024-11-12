# data preparation from Metro

# load libraries
library(dplyr)

# load original data
DADOSabril = read_delim("original/Dados de Abril 2024 (1 079 268 registos).csv", 
                                                      delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                                      trim_ws = TRUE)
DADOSabril = DADOSabril |>
  select(Dia, Hora, Título, `N. de série`, Entrada, Saída) |> 
  rename(Titulo = Título,
         ID = `N. de série`,
         Saida = Saída) |> 
  na.omit()


DADOSmaio = read_delim("original/Dados de Maio 2024 (1 129 439 registos).csv", 
                        delim = "|", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                        trim_ws = TRUE)
DADOSmaio = DADOSmaio |>
  select(Dia, Hora, Título, `N.º de Série`, Entrada, Saída) |> 
  rename(Titulo = Título,
         ID = `N.º de Série`,
         Saida = Saída) |> 
  na.omit()

DADOSmetro = rbind(DADOSabril, DADOSmaio)

# save data
write.csv(DADOSmetro, "original/DADOSmetro.csv", row.names = FALSE)
piggyback::pb_upload("original/DADOSmetro.csv")




# titulos transporte --------------------------------------------------------------------------

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
        grepl("Navegante", Texto, ignore.case = TRUE) ~ "Navegante Metropolitano", # inclui Urbano
        grepl("sub23", Texto, ignore.case = TRUE) ~ "Sub23",
        grepl("BT", Texto, ignore.case = TRUE) ~ "Carris Metropolitana",
        grepl("30", Texto, ignore.case = TRUE) ~ "Combinado Mensal",
        grepl("Trabalhador", Texto, ignore.case = TRUE) ~ "Trabalhador",
        grepl("24", Texto, ignore.case = TRUE) ~ "Diario",
        grepl("48H", Texto, ignore.case = TRUE) ~ "Diario",
        grepl("72", Texto, ignore.case = TRUE) ~ "Diario",
        grepl("Zapping", Texto, ignore.case = TRUE) ~ "Zapping",
        grepl("Bilhete", Texto, ignore.case = TRUE) ~ "Viagem",
        grepl("Viagem", Texto, ignore.case = TRUE) ~ "Viagem",
        grepl("Comb", Texto, ignore.case = TRUE) ~ "Combinado Mensal",
        TRUE ~ "Outro"
      )
  )

Titulos_simplex = Titulos |>
  select(Titulo, Tipo) |>
  distinct() |> 
  arrange(Tipo)

table(Titulos_simplex$Tipo)

# save data
write.csv(Titulos_simplex, "data/Titulos.csv", row.names = FALSE)
