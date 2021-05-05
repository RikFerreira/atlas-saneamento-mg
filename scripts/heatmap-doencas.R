library(tidyverse)
library(sf)
library(geobr)

mg_mun <- read_municipality(31)

base_gestao <- readxl::read_xlsx("data/ignored/base_gestao.xlsx", sheet = 2)

doencas <- base_gestao %>%
    select(
        code_muni = CodMun,
        diarreia = SMSBDG061901,
        leptospirose = SMSBDG061901,
        verminoses = SMSBDG061901,
        colera = SMSBDG061901,
        difteria = SMSBDG061901,
        dengue = SMSBDG061901,
        zika = SMSBDG061901,
        chikungunya = SMSBDG061901,
        tifo = SMSBDG061901,
        malaria = SMSBDG061901,
        hepatite = SMSBDG061901,
        febre_amarela = SMSBDG061901,
        dermatite = SMSBDG061901,
        doenca_respiratoria = SMSBDG061901,
        outras = SMSBDG061901
    ) %>%
    mutate_if(
        is.character,
        ~case_when(
            .x == "Sim" ~ TRUE,
            .x == "Não" ~ FALSE,
            .x == "-" ~ NA
        )
    ) %>%
    filter(as.character(code_muni) %>% str_detect("^31")) %>%
    pivot_longer(c(diarreia:outras), names_to = "doenca", values_to = "ocorre") %>%
    group_by(code_muni) %>%
    slice(1)

mg_mun %>%
    left_join(doencas, "code_muni") %>%
    ggplot() +
    geom_sf(aes(fill = ocorre))

plano <- base_gestao %>%
    select(
        code_muni = CodMun,
        plano = SMSBDG0603
    ) %>%
    filter(as.character(code_muni) %>% str_detect("^31"))

adequacao <- read_csv("output/moradia-adequada.csv")

mg_mun %>%
    left_join(plano, "code_muni") %>%
    left_join(adequacao, "code_muni") %>%
    ggplot() +
    geom_boxplot(aes(x = plano, y = MORADIA_ADEQ_PER, group = plano))
