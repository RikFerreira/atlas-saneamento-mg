library(tidyverse)
library(sf)
library(ggspatial)
library(patchwork)
library(Hmisc)
library(geobr)
library(RSQLite)
library(survey)
library(abjutils)

if("geobr" %in% .packages()) {
    br <- read_state()
    
    mg_mun <- read_municipality(31) %>%
        mutate(code_muni = as.character(code_muni))
    
    mg_rgint <- read_intermediate_region(31) %>%
        mutate(code_intermediate = as.character(code_intermediate))
} else {
    br <- st_read("/mnt/HDD/STORAGE/georesources/vector/MUNICIPIOS.gpkg", layer = "BR_UF_2019") %>%
        rename(code_state = cd_uf)
    
    mg_rgint <- st_read("/mnt/HDD/STORAGE/georesources/vector/MUNICIPIOS.gpkg", layer = "BR_RG_Intermediarias_2019") %>%
        rename(code_rgint = cd_rgint)
    
    mg_mun <- st_read("/mnt/HDD/STORAGE/georesources/vector/MUNICIPIOS.gpkg", layer = "BR_Municipios_2019") %>%
        filter(sigla_uf == "MG") %>%
        rename(code_muni = cd_mun)
}

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
        ADEQ = sum(ADEQ, na.rm = TRUE),
        INADEQ = sum(INADEQ, na.rm = TRUE)
    ) %>%
    mutate(
        ADEQ_PER = ADEQ / (ADEQ + INADEQ),
        INADEQ_PER = INADEQ / (ADEQ + INADEQ)
    )

final <- mg_mun %>%
    left_join(mg_agua, "code_muni") %>%
    # filter(Situacao_setor == "Urbano") %>%
    mutate(Freq = cut2(ADEQ_PER, g = 5))

main_map <- final %>%
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
    geom_sf(aes(fill = estados), size = .25) +
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
        title = "Total das despesas com saneamento por população (R$)",
        caption = " Fonte: SICONFI, 2019\n Org.: NEPRA, 2021\n SRC: SIRGAS 2000\n"
    ) &
    theme(
        plot.background = element_rect(fill = "#f5f6f1", size = 0),
        panel.background = element_rect(fill = "#f5f6f1", size = 0),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 16),
        text = element_text(family = "Fira Sans", size = 10)
    )
    
ggsave("plots/agua_adeq.png", plot, width = 297, height = 210, units = "mm", dpi = 300)

rgint <- final %>%
    left_join(
        readxl::read_xlsx("data/ignored/regioes-geograficas.xlsx") %>%
            mutate(
                nome_rgint = rm_accent(nome_rgint) %>% str_to_lower()
            ) %>%
            rename(code_muni = CD_GEOCODI) %>%
            select(code_muni, cod_rgint, nome_rgint),
        "code_muni"
    ) %>%
    mutate(name_muni = rm_accent(name_muni) %>% str_to_lower())

rgint %>%
    # filter(str_detect(name_muni, "Juiz De Fora"))
    st_drop_geometry() %>%
    # count(nome_rgint)
    mutate(
        is_center = ifelse(name_muni == nome_rgint, TRUE, FALSE)
    ) %>%
    count(is_center)

rgint %>%
    ggplot() +
    geom_boxplot(aes(x = nome_rgint, y = ADEQ_PER, group = nome_rgint))
