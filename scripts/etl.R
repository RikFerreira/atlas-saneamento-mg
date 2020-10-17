library(tidyverse)
library(survey)
library(PNADcIBGE)

micro_pnadc <- get_pnadc(
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

micro2019 %>%
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
