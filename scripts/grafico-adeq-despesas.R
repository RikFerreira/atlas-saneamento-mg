library(tidyverse)

moradia <- read_csv("output/moradia-adequada.csv")
componentes <- read_csv("output/saneamento-componentes.csv")
despesas <- read_csv("output/despesas.csv")
regioes <- readxl::read_xlsx("data/ignored/regioes-geograficas.xlsx") %>%
    select(code_muni = CD_GEOCODI, cod_rgint, nome_rgint)

final <- left_join(
    componentes, despesas,
    "code_muni"
) %>%
    mutate(code_muni = as.character(code_muni)) %>%
    left_join(
        regioes, "code_muni"
    )

final %>%
    drop_na() %>%
    .[,2:3] %>%
    cor()

final %>%
    pivot_longer(
        cols = c(AGUA_ADEQ:LIXO_ADEQ),
        names_to = "COMPONENTE",
        values_to = "Freq"
    ) %>%
    ggplot() +
    geom_point(aes(despesas, Freq), colour = "steelblue") +
    # geom_smooth(aes(despesas, MORADIA_ADEQ), method = "lm", se = FALSE, colour = "steelblue") +
    facet_wrap(~COMPONENTE) +
    scale_x_log10(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
    scale_y_log10(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
    theme_minimal() +
    ggsave("plots/saneamento-despesas.png", width = 10, height = 5, dpi = 300)
