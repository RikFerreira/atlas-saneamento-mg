library(tidyverse)
library(sf)
library(geobr)
library(patchwork)
library(ggspatial)

br <- read_state(year = 2019, simplified = FALSE)
mg <- read_state(31, year = 2019, simplified = FALSE)
rgint <- read_intermediate_region(31, year = 2019, simplified = FALSE)

main_map <- mg %>%
    ggplot() +
    geom_sf(size = .5, fill = NA) +
    geom_sf(data = rgint, size = .25, fill = NA) +
    geom_sf_label(data = rgint, aes(label = name_intermediate), size = 3) +
    annotation_north_arrow(style = north_arrow_fancy_orienteering()) +
    annotation_scale(pad_x = unit(140, "mm")) +
    labs(caption = "Fonte: IBGE, 2019\nOrg.: ALVES, R. F. 2021")

main_map

context_map <- br %>%
    mutate(estados = ifelse(code_state == "31", "Minas Gerais", "Demais estados")) %>%
    ggplot() +
    geom_sf(aes(fill = estados)) +
    scale_fill_manual("Estados", values = c("white", "firebrick3"))

layout <- c(
    area(1, 2, 1, 59),
    area(2, 1, 41, 1),
    area(2, 60, 41, 60),
    area(42, 2, 42, 59),
    area(3, 3, 40, 43),
    area(3, 45, 23, 58),
    area(26, 45, 40, 58)
)

plot(layout)

patch <- plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() +
    main_map + guide_area() + context_map +
    plot_layout(guides = "collect", design = layout) &
    theme_void() +
    theme(
        plot.background = element_rect(fill = "#efefee", size = 0),
        panel.background = element_rect(fill = "#efefee", size = 0)
    )

patch

ggsave("plots/mapa-de-localizacao.png", patch, device = "png", width = 297, height = 210, units = "mm")
