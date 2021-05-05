library(tidyverse)
library(sf)
library(ggspatial)
library(patchwork)
library(readxl)
library(geobr)

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

entorno02 <- read_xls("data/ignored/MG/Base informaЗoes setores2010 universo MG/EXCEL/Entorno02_MG.XLS")

adequacao <- entorno02 %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(
        code_muni = as.character(Cod_setor) %>% str_extract("^......."),
        SITUACAO = ifelse(Situacao_setor %in% c(1, 2, 3), "URBANO", "RURAL"),
        ADEQUADA = V202 + V203,
        SEMIADEQUADA = V204 + V205,
        INADEQUADA = V206 + V207
    ) %>%
    select(code_muni, ADEQUADA:INADEQUADA) %>%
    group_by(code_muni) %>%
    summarize_if(is.numeric, sum, na.rm = TRUE)

mg_adequacao <- left_join(mg_mun, adequacao, "code_muni")

siconfi <- read_csv("output/siconfi_reports.csv") %>%
    filter(
        exercicio == 2019 &
            coluna == "Despesas Pagas"
    )

mg_siconfi <- mg_mun %>%
    left_join(
        siconfi %>% mutate(cod_ibge = as.character(cod_ibge)),
        c("code_muni" = "cod_ibge")
    )

final <- mg_siconfi %>%
    # mutate(Freq = Hmisc::cut2(valor, g = 5)) # Valor
    mutate(Freq = Hmisc::cut2(round(valor / populacao, digits = -1), g = 5)) # Valor/população

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
    
ggsave("plots/despesas.png", plot, width = 297, height = 210, units = "mm", dpi = 300)

final %>%
    st_drop_geometry() %>%
    mutate(despesas = valor) %>%
    select(code_muni, despesas) %>%
    write_csv("output/despesas.csv", na = "")
