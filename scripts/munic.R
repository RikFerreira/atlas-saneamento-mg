library(tidyverse)
library(readxl)

munic_agua <- read_xlsx("data/ignored/base_abastecimento.xlsx", sheet = 2) %>%
    select(
        COD_UF = 4,
        COD_MUN = 1,
        SMSBAA0701:SMSBAA072121
    ) %>%
    mutate_if(
        is.character,
        ~{
            ifelse(.x == "Sim", TRUE,
            ifelse(.x == "Não", FALSE,
            ifelse(.x %in% c("-", "Não informou", "(*) Não soube informar"), NA, .x
            )))
        }
    ) %>%
    type_convert()

munic_esgoto <- read_xlsx("data/ignored/base_abastecimento.xlsx", sheet = 3) %>%
    select(
        COD_UF = 4,
        COD_MUN = 1,
        SMSBES0801:SMSBES081521
    ) %>%
    mutate_if(
        is.character,
        ~{
            ifelse(.x == "Sim", TRUE,
            ifelse(.x == "Não", FALSE,
            ifelse(.x %in% c("-", "Não informou", "(*) Não soube informar"), NA, .x
            ))) %>% type.convert()
        }
    )

munic_gestao <- read_xlsx("data/ignored/base_gestao.xlsx", sheet = 2) %>%
    mutate_if(
        is.character,
        ~{
            ifelse(.x == "Sim", TRUE,
            ifelse(.x == "Não", FALSE,
            ifelse(.x %in% c("-", "Não informou", "(*) Não soube informar"), NA, .x
            ))) %>% type.convert()
        }
    )
