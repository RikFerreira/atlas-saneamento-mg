library(tidyverse)
library(rsiconfi)
library(sf)
library(geobr)

municipalities <- read_municipality(31) %>%
    st_drop_geometry() %>%
    pull(code_muni) %>%
    append("31")
    as.character()

reports <- get_dca(
    year = 2014:2019,
    annex = "I-E",
    entity = municipalities,
    arg_cod_conta = 17
) %>%
    as_tibble()

reports %>%
    mutate(na = is.na(valor)) %>%
    count(exercicio, na)
