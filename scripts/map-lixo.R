library(tidyverse)
library(sf)
library(geobr)
library(ggspatial)
library(patchwork)
library(Hmisc)

br <- read_state()

mg_mun <- read_municipality(31) %>%
    mutate(code_muni = as.character(code_muni))

mg_rgint <- read_intermediate_region(31) %>%
    mutate(code_intermediate = as.character(code_intermediate))

mg_dom <- readxl::read_xls("data/ignored/MG/Base informaЗoes setores2010 universo MG/EXCEL/Domicilio01_MG.xls")

mg_lixo <- mg_dom %>%
    mutate_all(~ifelse(str_detect(., "^X$"), "", .)) %>%
    type_convert() %>%
    mutate(
        code_muni = str_extract(Cod_setor, "^......."),
        INADEQ = V037 + V038 + V039 + V040 + V041 + V042,
        ADEQ = V036,
        Situacao_setor = ifelse(Situacao_setor %in% c("1", "2", "3"), "Urbano", "Rural")
    ) %>%
    # select(code_muni, Situacao_setor, INADEQ) %>%
    group_by(code_muni) %>%
    summarise(
        LIXO_ADEQ = sum(ADEQ, na.rm = TRUE),
        LIXO_INADEQ = sum(INADEQ, na.rm = TRUE)
    ) %>%
    mutate(
        LIXO_ADEQ_PER = LIXO_ADEQ / (LIXO_ADEQ + LIXO_INADEQ),
        LIXO_INADEQ_PER = LIXO_INADEQ / (LIXO_ADEQ + LIXO_INADEQ)
    )

mg_shape <- mg_mun %>%
    mutate(code_muni = as.character(code_muni)) %>%
    left_join(mg_lixo, "code_muni") %>%
    mutate(
        LIXO_ADEQ_PER = LIXO_ADEQ_PER * 100,
        Freq = cut2(LIXO_ADEQ_PER, g = 5)
    )

main_map <- mg_shape %>%
    ggplot() +
    geom_sf(aes(fill = Freq), size = .25) +
    geom_sf(data = mg_rgint, aes(color = "black"), fill = NA, size = 1) +
    scale_fill_brewer(palette = "Reds", na.value = "lightgray") +
    scale_color_identity(guide = "legend", name = NULL, breaks = "black", labels = "Regiões intermediárias") +
    fixed_plot_aspect(40/38) +
    annotation_north_arrow(
        width = unit(.6, "cm"),
        height = unit(1.2, "cm"),
        pad_x = unit(11.4, "cm"),
        pad_y = unit(1.5, "cm")
    ) +
    annotation_scale(
        pad_x = unit(10, "cm"),
        pad_y = unit(1, "cm")
    ) +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "#f5f6f1", size = 0),
        panel.background = element_rect(fill = "#f5f6f1", size = 0),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 16),
        axis.ticks = element_line(size = .5),
        axis.ticks.length = unit(.15, "cm"),
        axis.text = element_text(),
        axis.text.y = element_text(angle = 90)
    )

context_map <- br %>%
    mutate(estados = ifelse(code_state == "31", "Minas Gerais", "Demais estados")) %>%
    ggplot() +
    geom_sf(aes(fill = estados), size = .25, show.legend = FALSE) +
    scale_fill_manual("Estados", values = c("white", "firebrick3"), na.value = "darkgray") +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "#f5f6f1", size = 0),
        panel.background = element_rect(fill = "#f5f6f1", size = 0),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 16)
    )

single_choroplet_landscape <- c(
    area(1, 2, 1, 59),
    area(2, 1, 41, 1),
    area(2, 60, 41, 60),
    area(42, 2, 42, 59),
    area(3, 3, 40, 42),
    area(3, 45, 23, 58),
    area(26, 45, 40, 58)
)

plot <- plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() +
    main_map + guide_area() + context_map +
    plot_layout(guides = "collect", design = single_choroplet_landscape) +
    plot_annotation(
        title = "Domicílios com coleta de lixo adequada (%)",
        caption = " Fonte: IBGE, 2019\n Org.: NEPRA, 2021\n SRC: SIRGAS 2000\n"
    ) &
    theme(
        plot.background = element_rect(fill = "#f5f6f1", size = 0),
        panel.background = element_rect(fill = "#f5f6f1", size = 0),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 16),
        text = element_text(family = "Fira Sans", size = 10)
    )

ggsave("plots/lixo.png", plot, width = 297, height = 210, units = "mm", dpi = 300)
