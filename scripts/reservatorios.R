library(tidyverse)
library(reservatoriosBR)

reservatoriosBR::tabela_reservatorios() %>%
    filter(estado_1 == "MG" | estado_2 == "MG") %>%
    view()
