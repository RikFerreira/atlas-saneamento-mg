library(tidyverse)
library(survey)
library(PNADcIBGE)
library(RPostgreSQL)

micro2019 <- get_pnadc(
    year = 2019,
    interview = 1,
    vars = c(
        "UPA", "V1008",
        "V1031", "V1032", "posest",
        "V1022", "S01010", "S01012A", "S01013"
    ),
    labels = FALSE,
    design = FALSE
)

design2019 <- micro2019 %>%
    mutate(
        S01012A = replace_na(S01012A, "99"),
        SITUACAO = ifelse(V1022 == "1", "Urbano", "Rural") %>% as_factor() %>% fct_relevel("Urbano", "Rural"),
        DOMICILIO = paste0(UPA, V1008),
        ADEQUACAO = ifelse(
            S01010 == 1 & (S01012A == 1 | S01012A == 2) & S01013 == 1,
            "Adequado", ifelse(
                S01010 == 1 | (S01012A == 1 | S01012A == 2) | S01013 == 1,
                "Semi-adequado", "Inadequado"
            )
        ) %>%
            as_factor() %>%
            fct_relevel(c("Adequado", "Semi-adequado", "Inadequado"))
    ) %>%
    pnadc_design()

svytable(~ UF + SITUACAO + ADEQUACAO + S01010 + S01012A + S01013, design2019) %>%
    as_tibble() %>%
    rename(
        AGUA = S01010,
        ESGOTO = S01012A,
        LIXO = S01013
    ) %>%
    mutate(
        AGUA = ifelse(AGUA == "1", "Adequado", "Inadequado"),
        ESGOTO = ifelse(ESGOTO %in% c("1", "2"), "Adequado", "Inadequado"),
        LIXO = ifelse(LIXO == "1", "Adequado", "Inadequado")
    ) %>%
    group_by(UF, SITUACAO, ADEQUACAO, AGUA, ESGOTO, LIXO) %>%
    summarise(n = sum(n, na.rm = TRUE) %>% round()) %>%
    ungroup() %>%
    write_csv("../output/saneamento_ufs.csv")
