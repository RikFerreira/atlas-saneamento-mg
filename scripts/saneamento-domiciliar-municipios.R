library(tidyverse)
library(corrplot)
library(geobr)
library(sf)
library(biscale)
library(cowplot)
library(patchwork)
library(ggspatial)

br <- read_state()

mg_mun <- read_municipality(31) %>%
    mutate(code_muni = as.character(code_muni))

mg_rgint <- read_intermediate_region(31) %>%
    mutate(code_intermediate = as.character(code_intermediate))

mg_dom <- readxl::read_xls("data/ignored/MG/Base informaЗoes setores2010 universo MG/EXCEL/Domicilio01_MG.xls")

mg_agua <- mg_dom %>%
    mutate_all(~ifelse(str_detect(., "^X$"), "", .)) %>%
    type_convert() %>%
    mutate(
        code_muni = str_extract(Cod_setor, "^......."),
        INADEQ = V013 + V014 + V015,
        ADEQ = V012,
        Situacao_setor = ifelse(Situacao_setor %in% c("1", "2", "3"), "Urbano", "Rural")
    ) %>%
    # select(code_muni, Situacao_setor, INADEQ) %>%
    group_by(code_muni) %>%
    summarise(
        AGUA_ADEQ = sum(ADEQ, na.rm = TRUE),
        AGUA_INADEQ = sum(INADEQ, na.rm = TRUE)
    ) %>%
    mutate(
        AGUA_ADEQ_PER = AGUA_ADEQ / (AGUA_ADEQ + AGUA_INADEQ),
        AGUA_INADEQ_PER = AGUA_INADEQ / (AGUA_ADEQ + AGUA_INADEQ)
    )

mg_esgoto <- mg_dom %>%
    mutate_all(~ifelse(str_detect(., "^X$"), "", .)) %>%
    type_convert() %>%
    mutate(
        code_muni = str_extract(Cod_setor, "^......."),
        INADEQ = V019 + V020 + V021 + V022 + V023,
        ADEQ = V017 + V018,
        Situacao_setor = ifelse(Situacao_setor %in% c("1", "2", "3"), "Urbano", "Rural")
    ) %>%
    # select(code_muni, Situacao_setor, INADEQ) %>%
    group_by(code_muni) %>%
    summarise(
        ESGOTO_ADEQ = sum(ADEQ, na.rm = TRUE),
        ESGOTO_INADEQ = sum(INADEQ, na.rm = TRUE)
    ) %>%
    mutate(
        ESGOTO_ADEQ_PER = ESGOTO_ADEQ / (ESGOTO_ADEQ + ESGOTO_INADEQ),
        ESGOTO_INADEQ_PER = ESGOTO_INADEQ / (ESGOTO_ADEQ + ESGOTO_INADEQ)
    )

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

mg_agua
mg_esgoto
mg_lixo

mg_final <- plyr::join_all(list(mg_agua, mg_esgoto, mg_lixo), by = "code_muni", type = "left") %>%
    as_tibble()

mg_final %>%
    select(code_muni, ends_with("_ADEQ_PER")) %>%
    select(-code_muni) %>%
    cor() %>%
    corrplot(method = "square")

mg_shape <- mg_mun %>%
    mutate(code_muni = as.character(code_muni)) %>%
    left_join(mg_final, "code_muni")

map <- mg_shape %>%
    bi_class(AGUA_ADEQ, ESGOTO_ADEQ, style = "quantile", dim = 3) %>%
    ggplot() +
    geom_sf(aes(fill = bi_class), size = .25, show.legend = FALSE) +
    geom_sf(data = mg_rgint, aes(color = "black"), fill = NA, size = 1, show.legend = FALSE) +
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
    bi_scale_fill(pal = "DkViolet", dim = 3) +
    bi_theme() +
    theme(
        axis.ticks = element_line(size = .5),
        axis.ticks.length = unit(.15, "cm"),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(angle = 90)
    )

legend <- bi_legend(
    pal = "DkViolet",
    dim = 3,
    xlab = "Água adequada",
    ylab = "Esgotamento adequado",
    size = 8
)

main_map <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, .2, .65, .2, .2)

mg_shape %>%
    bi_class(AGUA_ADEQ, ESGOTO_ADEQ, style = "quantile", dim = 3) %>%
    pull(bi_class) %>%
    table()
    
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
    map + legend + context_map +
    plot_layout(guides = "collect", design = single_choroplet_landscape) +
    plot_annotation(
        title = "Domicílios com acesso a agua e esgoto adequados (quantil %)",
        caption = " Fonte: IBGE, 2010\n Org.: NEPRA, 2021\n SRC: SIRGAS 2000\n"
    ) &
    theme(
        plot.background = element_rect(fill = "#f5f6f1", size = 0),
        panel.background = element_rect(fill = "#f5f6f1", size = 0),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 16),
        text = element_text(family = "Fira Sans", size = 10)
    )

ggsave("plots/agua-esgoto.png", plot, width = 297, height = 210, units = "mm", dpi = 300)

mg_shape %>%
    st_drop_geometry() %>%
    select(code_muni, AGUA_ADEQ, ESGOTO_ADEQ, LIXO_ADEQ) %>%
    write_csv("output/saneamento-componentes.csv")

mg_shape %>%
    st_drop_geometry() %>%
    select(code_muni, AGUA_ADEQ_PER, ESGOTO_ADEQ_PER, LIXO_ADEQ_PER) %>%
    pivot_longer(cols = c(AGUA_ADEQ_PER, ESGOTO_ADEQ_PER, LIXO_ADEQ_PER)) %>%
    mutate(class = ifelse(value < .333, 33, ifelse(value < .667, 66, 100))) %>%
    count(class, name) %>%
    arrange(class)
