library(tidyverse)
library(sf)
library(geobr)
library(patchwork)
library(ggspatial)

br <- read_state(year = 2019, simplified = FALSE)
mg <- read_state(31, year = 2019, simplified = FALSE)
rgint <- read_intermediate_region(31)

main_map <- read_csv("data/declaracoes-igam.csv") %>%
# read_csv("data/declaracoes-igam.csv") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4674) %>%
    mutate(Estações = "Estação com restrição de uso") %>%
    ggplot() +
    geom_sf(aes(color = Estações), size = 3) +
    geom_sf(data = rgint, size = .25, fill = NA) +
    geom_sf_text(data = rgint, aes(label = name_intermediate), size = 3) +
    scale_color_manual(values = "steelblue") +
    annotation_north_arrow(style = north_arrow_fancy_orienteering()) +
    annotation_scale(pad_x = unit(140, "mm")) +
    labs(caption = "Fonte: IGAM, 2021\nOrg.: ALVES, R. F. 2021\nSRC: SIRGAS 2000")

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

patch <- plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() +
    main_map + guide_area() + context_map +
    plot_layout(guides = "collect", design = layout) &
    theme_void() +
    theme(
        plot.background = element_rect(fill = "#efefee", size = 0),
        panel.background = element_rect(fill = "#efefee", size = 0)
    )

# patch

ggsave("plots/igam.png", patch, device = "png", width = 297, height = 210, units = "mm")
