library(tidyverse)
library(survey)
library(PNADcIBGE)
library(RPostgreSQL)
library(survey)

#2010 data
# Connection to PostgreSQL database
conn <- dbConnect(
    dbDriver("PostgreSQL"),
    user = "postgres",
    password = readline(prompt = "Senha: "),
    host = "localhost",
    port = 5432,
    dbname = "censo_pessoas_2010"
)

# Input
micro2010_raw <- dbGetQuery(conn, readLines("../scripts/saneamento_censo_2010.sql")) %>%
    as_tibble()

# Reclassification
micro2010 <- micro2010_raw %>%
    mutate(
        SITUACAO = ifelse(V1006 == "1", "Urbano", "Rural") %>% as_factor() %>% fct_relevel("Urbano", "Rural"),
        AGUA = ifelse(V0208 == "01", "Adequado", "Inadequado") %>% as_factor() %>% fct_relevel(c("Adequado", "Inadequado")),
        ESGOTO = ifelse(V0207 == "1" | V0207 == "2", "Adequado", "Inadequado") %>% as_factor() %>% fct_relevel(c("Adequado", "Inadequado")),
        LIXO = ifelse(V0210 == "1", "Adequado", "Inadequado") %>% as_factor() %>% fct_relevel(c("Adequado", "Inadequado")),
        ADEQUACAO = ifelse(
            AGUA == "Adequado" & ESGOTO == "Adequado" & LIXO == "Adequado", "Adequado",
            ifelse(
                AGUA == "Adequado" | ESGOTO == "Adequado" | LIXO == "Adequado", "Semi-adequado",
                    "Inadequado"
            )
        ) %>% as_factor() %>% fct_relevel(c("Adequado", "Semi-adequado", "Inadequado"))
    ) %>%
    select(
        UF = V0001, SITUACAO, ADEQUACAO, AGUA, ESGOTO, LIXO, PESO = V0010
    ) %>%
    group_by(UF) %>%
    mutate(fpc = n()) %>%
    ungroup()

design2010 <- svydesign(
    ids = ~1,
    strata = ~UF,
    fpc = ~fpc,
    weights = ~PESO,
    data = micro2010
)

# Output
saneamento2010 <- svytable(~ UF + SITUACAO + ADEQUACAO + AGUA + ESGOTO + LIXO, design2010) %>%
    as_tibble() %>%
    group_by(UF, SITUACAO, ADEQUACAO, AGUA, ESGOTO, LIXO) %>%
    summarise(n = sum(n, na.rm = TRUE) %>% round()) %>%
    ungroup()

# 2019 data
# Input
micro2019_raw <- get_pnadc(
    year = 2019,
    interview = 1,
    vars = c(
        "UPA", "V1008", "V2005",
        "V1030", "V1031", "V1032", "posest",
        "V1022", "S01010", "S01012A", "S01013"
    ),
    labels = FALSE,
    design = FALSE
)

# Reclassification
micro2019 <- micro2019_raw %>%
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
    )

design2019 <- micro2019 %>%
    pnadc_design()

# Output
saneamento2019 <- svytable(~ UF + SITUACAO + ADEQUACAO + S01010 + S01012A + S01013, subset(design2019, V2005 == "01")) %>%
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
    ungroup()

# Saving
bind_rows(saneamento2010, saneamento2019) %>%
    write_csv("../output/saneamento_domiciliar.csv")
